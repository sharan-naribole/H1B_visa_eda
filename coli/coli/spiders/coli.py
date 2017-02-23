import scrapy
from coli.items import ColiItem
import pandas as pd

class coliSpider(scrapy.Spider):
    name = "coli"

    allowed_domains = ["numbeo.com"]
    start_urls = ['https://www.numbeo.com/cost-of-living/country_result.jsp?country=United+States'] #top submissions in the past week

    def parse(self,response):
        self.logger.info("Visited %s",response.url)

        # City and State Scrape
        city = []
        state = []
        for location in response.css('.discreet_link::text').extract():
            #Split city and state_code
            print(location)
            city_curr, state_curr = location.split(',')
            city.append(city_curr)
            state.append(state_curr)

        # Cost of living Scrape
        coli = []
        for coli_curr in response.css('td:nth-child(5)::text').extract():
            #Cost of Living plus Rent Index
            print(coli_curr)
            coli.append(float(coli_curr))

        # Merging data
        coli_data = { 'city': city,
                      'state': state,
                      'coli': coli }
        coli_df = pd.DataFrame(coli_data)
        coli_df.to_csv('coli_data.csv')
