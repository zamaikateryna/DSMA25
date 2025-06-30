select j ->> 'business_id' as business_id,
  array_length(string_to_array(trim(trailing ',' from j ->> 'date'), ','), 1) as checkin_count
from checkin
where j ->> 'business_id' IN (
  select j ->> 'business_id'
  from business
  where j ->> 'categories' like '%Restaurants%' and
   j ->> 'categories' like '%American (Traditional)%' );