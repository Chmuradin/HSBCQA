import re
from io import StringIO
from datetime import datetime, timedelta
import requests
import pandas as pd


class YahooFinanceHistory:
    """A class used to scrap historical data from finance.yahoo.com
    ...
    Constant attributes:
    ------------
    timeout: int
        time for establishing connections to a server
    headers: dict
        required by yahoo server to access data
    crumb_link, quote_link: str
        links to the service
    crumble_regex: str/regex
        find user's current location on a website

    Attributes:
    ----------
    symbol: str
        index to be scrapped
    days_range: int
        amount of days we want to scrap data,counted from today

    Methods:
    ---------
    get_crumb():
        returns crumb for the website
    get_quote():
        returns dataframe with data
    """
    timeout = 5
    headers = {'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10.10; rv:39.0)'}
    crumb_link = 'https://finance.yahoo.com/quote/{0}/history?p={0}'
    crumble_regex = r'CrumbStore":{"crumb":"(.*?)"}'
    quote_link = 'https://query1.finance.yahoo.com/v7/finance/download/{quote}?period1={dfrom}&period2={dto}&interval=1d&events=history&crumb={crumb}'

    def __init__(self, symbol, days_range=7):
        self.symbol = symbol
        self.session = requests.Session()
        self.dt = timedelta(days=days_range)
        self.crumb = ""

    def get_crumb(self):
        response = self.session.get(self.crumb_link.format(self.symbol), timeout=self.timeout, headers=self.headers)
        response.raise_for_status()
        match = re.search(self.crumble_regex, response.text)
        if not match:
            raise ValueError('No crumb')
        else:
            self.crumb = match.group(1)

    def get_quote(self):
        if not hasattr(self, 'crumb') or len(self.session.cookies) == 0:
            self.get_crumb()
        date_to = int(datetime.utcnow().timestamp())
        date_from = int((datetime.utcnow() - self.dt).timestamp())
        url = self.quote_link.format(quote=self.symbol, dfrom=date_from, dto=date_to, crumb=self.crumb)
        response = self.session.get(url, headers=self.headers)
        response.raise_for_status()
        return pd.read_csv(StringIO(response.text), parse_dates=['Date'])


for index in ['DAX', 'FTSEMIB.MI', '^GSPC', 'WIG20.WA']:
    df = YahooFinanceHistory(index, days_range=1826).get_quote()
    df.to_csv(f'{index}_5year_data.csv', index=False)
