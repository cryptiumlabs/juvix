#!/usr/bin/env python

import requests, sys

kb = 100

num_bytes = kb * 1000
price_ar = requests.get('https://arweave.net/price/{}'.format(num_bytes)).json() / 1000000000000.

print('Price to store {} KB:'.format(kb))
print('In AR tokens: {}'.format(price_ar))
