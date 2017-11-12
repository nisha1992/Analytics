#This script was run on a server to collect flights information in JSON Format through Google Flight QPX API. Data was collected almost for a month. Another R program is available in the following path : Analytics/Flights Pricing Analysis/DataCollectfromJSON.R for extracting data from JSON files.
#Some points regarding collected Data. 1. Flights data is available only for following airlines : Vistara, Jet Airways, Air India 2. Data is available only for non stop flights 3. Source and Destination airports are only DEL and BOM.


#!/bin/bash
TOC=$(date "+%Y_%m_%d_%H")
for i in {0..14}
do
a=$(date "-v""+""$i""d" "+%Y-%m-%d")
curl -X POST "https://www.googleapis.com/qpxExpress/v1/trips/search?key=AIzaSyBPYv86BoakH25UDDuIB59hlHEWuytoz6Q&alt=json" -H "Content-Type: application/json; charset=UTF-8" -d "{ \"request\": { \"slice\": [ { \"origin\": \"DEL\", \"destination\": \"BOM\",\"maxStops\": 0, \"date\": \"${a}\" } ], \"passengers\": { \"adultCount\": 1, \"infantInLapCount\": 0, \"infantInSeatCount\": 0, \"childCount\": 0, \"seniorCount\": 0 }, \"solutions\": 500, \"refundable\": false }}" > "flights_data_${TOC}_${i}.json"
done
~
