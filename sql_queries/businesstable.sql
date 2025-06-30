select j ->> 'city' as city, j->> 'name' as name, j ->> 'latitude' as latitude, j ->> 'longitude' as longitude, 
 cast(j ->>'stars' as dec(3,2)) as stars, j ->> 'state' as state, j ->> 'address' as address,
	   j->'attributes' ->> 'WiFi' as WiFi, j->'attributes' ->> 'RestaurantsDelivery' as RestaurantsDelivery,
	   j->'attributes' ->> 'RestaurantsGoodForGroups' as RestaurantsGoodForGroups, j->'attributes' ->> 'RestaurantsPriceRange2' as RestaurantsPriceRange, j->'attributes' ->> 'BusinessParking' as BusinessParking,
	   j ->> 'business_id' as business_id,
	   j ->> 'postal_code' as postal_code,
	   j ->> 'review_count' as review_count
from business
where j ->> 'categories' like '%Restaurants%' and
   j ->> 'categories' like '%American (Traditional)%' 
;