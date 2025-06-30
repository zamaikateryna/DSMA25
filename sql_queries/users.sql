select j ->> 'user_id' as user_id, j->>'review_count' as review_count,
j->>'useful' as useful, j->>'average_stars' as average_stars
from users
where j ->> 'user_id' in
(select j ->> 'user_id'
from review
where j ->> 'business_id' in (select j ->> 'business_id' as business_id
from business
where j->> 'categories' like '%Restaurants%' and
j->> 'categories' like '%American (Traditional)%'));