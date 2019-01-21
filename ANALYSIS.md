# Epic - Pricing

## *As a manager, I want the ability to control pricing of items, so that I can manage profit margin and inventory.*

### Acceptance Criteria

**Given** a collection of items
**And** a new item containing an item code, description, type, and price
**When** adding the item
**Then** the collection contains the item.

**Given** a collection of items
**And** an invalid item missing an item code, description, type, or price
**When** adding the item
**Then** an error is raised.

**Given** a collection of items
**And** a duplicate item with an item code, description, type, and price
**When** adding the item
**Then** the collection contains only the latest version of the item.

## *As a manager, I want the ability to temporarily modify pricing rules, so that I can run marketting campaigns.*

### Acceptance Criteria

> sale price = $1.00 each

**Given** a collection of pricing rules
**And** a new rule containing a start date, end date, item code, and price
**When** adding the rule
**Then** the collection contains the rule.

> 3 for $5.00

**Given** a collection of pricing rules
**And** a new rule containing a start date, end date, item code, quantity, and price
**When** adding the rule
**Then** the collection contains the rule.

> buy 2 get 1 for 50% off

**Given** a collection of pricing rules
**And** a new rule containing a start date, end date, item code, bulk quantity, sale quantity, and a discount percentage
**When** adding the rule
**Then** the collection contains the rule.

> buy 3 get 1 for 100% off limit 8

**Given** a collection of pricing rules
**And** a new rule containing a start date, end date, item code, bulk quantity, sale quantity, discount percentage, and a limit quantity
**When** adding the rule
**Then** the collection contains the rule.

> buy 2 get 1 for $1.00

**Given** a collection of pricing rules
**And** a new rule containing a start date, end date, item code, bulk quantity, sale quantity, and price
**When** adding the rule
**Then** the collection contains the rule.

> buy 2 get 1 for $1.00 limit 6

**Given** a collection of pricing rules
**And** a new rule containing a start date, end date, item code, bulk quantity, sale quantity, price, and a limit quantity
**When** adding the rule
**Then** the collection contains the rule.

> buy 2 packages of (by weight item) get 1 package of equal or lesser value for 50% off

**Given** a collection of pricing rules
**And** a new rule containing a start date, end date, item code, bulk quantity, sale quantity, and discount percentage
**When** adding the rule
**Then** the collection contains the rule.

> rule validation

**Given** a collection of pricing rules
**And** a rule that applies to `by quantity` items
**And** a matching item by item code with the item type `by weight`
**When** validating the rule
**Then** an error is raised.

**Given** a collection of pricing rules
**And** a rule that applies to `by weight` items
**And** a matching item by item code with the item type `by quantity`
**When** validating the rule
**Then** and error is raised.

**Given** a collection of pricing rules
**And** a rule with an item code
**And** no matching item by item code
**When** validating the rule
**Then** an error is raised.

> multiple discounts for an item

**Given** a collection of pricing rules
**And** a rule containing AT LEAST a start date, end date, and item code
**And** a rule exists with the same item code and an overlapping date range
**When** adding the rule
**Then** an error is raised
**And** the new rule is not included in the collection.

**Given** a collection of pricing rules
**And** an invalid rule (not matching the above rule types)
**When** adding the rule
**Then** and error is raised
**And** the rule is not included in the collection.

# Epic - Scanning

## *As a cashier, I want the ability to scan an item, so that items can be easily added to the transaction.*

### Acceptance Criteria

**Given** a transaction
**And** a new item code
**And** the item type is `by quantity`
**When** adding the item
**Then** the transaction contains the item
**And** the quantity is 1.

**Given** a transaction
**And** a duplicate item code
**And** the item type is `by quantity`
**When** adding the item
**Then** the transaction contains the item
**And** the quantity is increased by 1.

## *As a cashier, I want the ability to weigh an item that is sold by weight, so that I can quickly add it to the transaction.*

### Acceptance Criteria

**Given** a transaction
**And** a new item code
**And** the item type is `by weight`
**And** the weight is unknown
**When** adding the item
**Then** an error is raised.

**Given** a transaction
**And** a new item code
**And** the item type is `by weight`
**And** the weight is known
**When** adding the item
**Then** the transaction contains the item.

**Given** a transaction
**And** a duplicate item code
**And** the item type is `by weight`
**And** the weight is known
**When** adding the item
**Then** the transaction contains the existing duplicate items
**And** the transaction contains the new duplicate item.

## *As a cashier, I want the ability to void an item, so that items can be easily removed from the transaction.*

### Acceptance Criteria

**Given** a transaction
**And** a new item code
**When** removing the item
**Then** an error is raised.

**Given** a transaction
**And** an existing item code
**And** the item type is `by quantity`
**And** the existing quantity is 1
**When** removing the item
**Then** the transaction no longer has the item.

**Given** a transaction
**And** an existing item code
**And** the item type is `by quantity`
**And** the existing quantity is 2 or more
**When** removing the item
**Then** the transaction contains the item
**And** the quantity is reduced by 1.

**Given** a transaction
**And** an existing item code
**And** the item type is `by weight`
**And** the weight is unknown
**When** removing the item
**Then** an error is raised.

**Given** a transaction
**And** an existing item code
**And** the item type is `by weight`
**And** the weight is known
**When** removing the item
**Then** the transaction no longer contains the item.

# Epic - Calculating

## *As a customer, I want the ability to see the original price per item, so that I can be sure I am not overcharged.*

### Acceptance Criteria

**Given** a transaction
**And** an item not covered by a discount rule
**And** the item type is `by quantity`
**When** calculating the item total
**Then** the item total equals the item quantity times the item price.

**Given** a transaction
**And** an item not covered by a discount rule
**And** the item type is `by weight`
**When** calculating the item total
**Then** the item total equals the item weight times the item price per ounce.

## *As a customer, I want to see the discounted price for sale items, so that I can be sure I am getting the advertised sale price.*

### Acceptance Criteria

> sale price = $1.00 each

**Given** a transaction
**And** an item with the item type `by quantity`
**And** a discount rule for the same item code
**And** the discount rule contains a start date, end date, item code, and discount price
**And** today's date is between the start date and end date inclusive
**When** calculating the item total
**Then** the item total is the item quantity times the discount price.

> 3 for $5.00

**Given** a transaction
**And** an item with the item type `by qauntity`
**And** a discount rule for the same item code
**And** the discount rule contains a start date, end date, item code, quantity, and discount price
**And** the item quantity equals or exceeds the discount quantity
**And** today's date is between the start date and end date inclusive
**When** calculating the item total
**Then** the item total is the sum of the floor of the item quantity divided by the discount quantity times the discount price and the item quantity modula the discount quantity times the item price.

```
(floor(item_quantity / discount_quantity) * discount_price) + ((item_quantity % discount_quantity) * item_price)

item_quantity = 5
item_price = $2.00
discount_quantity = 3
discount_price = $5.00

(floor(5 / 3) * $5.00) + ((5 % 3) * $2.00)
(1 * $5.00) + (2 * $2.00)
$5.00 + $4.00
$9.00
```

> buy 2 get 1 for 50% off

**Given** a transaction
**And** an item with the item type `by quantity`
**And** a discount rule for the same item code
**And** the discount rule contains a start date, end date, item code, bulk quantity, sale quantity, and discount percentage
**And** the item quantity equals or exceeds the discount quantity
**And** today's date is between the start date and end date inclusive
**When** calculating the item total
**Then** the item total is the sum of the floor of the item quantity divided by the sum of the bulk quantity and the sale quantity times the sum of the bulk quantity times the item price and the sale quantity times the item price times the inverse of the discount percentage and the item quantity modula the sum of the discount quantity and the sale quantity times the item price.

```
(floor(item_quantity / (bulk_quantity + sale_quantity)) * ((bulk_quantity * item_price) + (sale_quantity * item_price * (100 - discount_percentage)))) + ((item_quantity % (bulk_quantity + sale_quantity)) * item_price)

item_quantity = 7
item_price = $10.00
bulk_quantity = 2
sale_quantity = 1
discount_percentage = 50%

(floor(7 / (2 + 1)) * ((2 * $10.00) + (1 * $10.00 * (100 - 50%)))) + ((7 % (2 + 1)) * $10.00)
(floor(7 / 3) * ($20.00 + (1 * $10.00 * 0.5))) + ((7 % 3) * $10.00)
(2 * ($20.00 + $5.00)) + (1 * $10.00)
(2 * $25.00) + $10.00
$50.00 + $10.00
$60.00
```

> buy 3 get 1 for 100% off limit 8

**Given** a transaction
**And** an item with the item type `by quantity`
**And** a discount rule for the same item code
**And** the discount rule contains a start date, end date, item code, bulk quantity, sale quantity, discount percentage, and maximum discount quantity
**And** the item quantity equals or exceeds the discount quantity
**And** today's date is between the start date and end date inclusive
**When** calculating the item total
**Then** the item total is calculated using the algorithm provided below.

```
item_quantity = 11
item_price = $10.00
bulk_quantity = 3
sale_quantity = 1
discount_percentage = 100%
max_discount_quantity = 8

over_quantity = (item_quantity % max_discount_quantity)
under_quantity = (item_quantity - over_quantity)
discount_quantity = (bulk_quantity + sale_quantity)

over_quantity = 11 % 8
over_quantity = 3

under_quantity = 11 - 3
under_quantity = 8

discount_quantity = 3 + 1
discount_quantity = 4

(floor(under_quantity / discount_quantity) * ((bulk_quantity * item_price) + (sale_quantity * item_price * (100 - discount_percentage)))) + (((under_quantity % discount_quantity) + over_quantity) * item_price)

(floor(8 / 4) * ((3 * $10.00) + (1 * $10.00 * (100 - 100%)))) + (((8 % 4) + 3) * $10.00)
(2 * ($30.00 + (1 * $10.00 * 0.0))) + ((0 + 3) * $10.00)
(2 * ($30.00 + $0.00)) + (3 * $10.00)
(2 * $30.00) + $30.00
$60.00 + $30.00
$90.00
```

> buy 2 get 1 for $1.00

**Given** a transaction
**And** an item with the item type `by quantity`
**And** a discount rule for the same item code
**And** the discount rule contains a start date, end date, item code, bulk quantity, sale quantity, and discount price
**And** the item quantity equals or exceeds the discount quantity
**And** today's date is between the start date and end date inclusive
**When** calculating the item total
**Then** the item total is the sum of the floor of the item quantity divided by the sum of the bulk quantity and the sale quantity times the sum of the bulk quantity times the item price and the sale quantity times the discount price and the item quantity modula the sum of the discount quantity and the sale quantity times the item price.

```
(floor(item_quantity / (bulk_quantity + sale_quantity)) * ((bulk_quantity * item_price) + (sale_quantity * discount_price))) + ((item_quantity % (bulk_quantity + sale_quantity)) * item_price)

item_quantity = 7
item_price = $10.00
bulk_quantity = 2
sale_quantity = 1
discount_price = $1.00

(floor(7 / (2 + 1)) * ((2 * $10.00) + (1 * $1.00))) + ((7 % (2 + 1)) * $10.00)
(floor(7 / 3) * ($20.00 + $1.00)) + ((7 % 3) * $10.00)
(2 * $21.00) + (1 * $10.00)
$42.00 + $10.00
$52.00
```

> buy 3 get 1 for $1.00 limit 8

**Given** a transaction
**And** an item with the item type `by quantity`
**And** a discount rule for the same item code
**And** the discount rule contains a start date, end date, item code, bulk quantity, sale quantity, discount price, and maximum discount quantity
**And** the item quantity equals or exceeds the discount quantity
**And** today's date is between the start date and end date inclusive
**When** calculating the item total
**Then** the item total is calculated using the algorithm provided below.

```
item_quantity = 11
item_price = $10.00
bulk_quantity = 3
sale_quantity = 1
discount_price = $1.00
max_discount_quantity = 8

over_quantity = (item_quantity % max_discount_quantity)
under_quantity = (item_quantity - over_quantity)
discount_quantity = (bulk_quantity + sale_quantity)

over_quantity = 11 % 8
over_quantity = 3

under_quantity = 11 - 3
under_quantity = 8

discount_quantity = 3 + 1
discount_quantity = 4

(floor(under_quantity / discount_quantity) * ((bulk_quantity * item_price) + (sale_quantity * item_price * discount_price))) + (((under_quantity % discount_quantity) + over_quantity) * item_price)

(floor(8 / 4) * ((3 * $10.00) + (1 * $1.00))) + (((8 % 4) + 3) * $10.00)
(2 * ($30.00 + $1.00)) + ((0 + 3) * $10.00)
(2 * $31.00) + (3 * $10.00)
$62.00 + $30.00
$92.00
```

> buy 2 packages of (by weight item) get 1 package of equal or lesser value for 50% off

**Given** a transaction
**And** an item with the item type `by weight`
**And** a discount rule for the same item code
**And** the discount rule contains a start date, end date, item code, bulk quantity, sale quantity, and a discount percentage
**And** the item quantity equals or exceeds the discount quantity
**And** today's date is between the start date and end date inclusive
**When** calculating the item total
**Then** the item total is calculated using the algorithm provided below.

```
collect all items matching item code
and sort items from highest cost (item_weight * item_price) to lowest cost

for each item:
    let index = item number (one based) modula (bulk_quantity + sale_quantity)
    if index is between 1 and bulk_quantity inclusive then add (item_weight * item_price) to item_total
    otherwise add (item_weight * item_price * discount_percentage) to item_total

bulk_quantity = 2
sale_quantity = 1
discount_percentage = 50%

collected items:
5 ounces at $0.30 per ounce ($1.50)
10 ounces at $0.25 per ounce ($2.50)
10 ounces at $0.10 per ounce ($1.00)
12 ounces at $0.20 per ounce ($2.40)

sorted items:
10 ounces at $0.25 per ounce ($2.50)
12 ounces at $0.20 per ounce ($2.40)
5 ounces at $0.30 per ounce ($1.50)
10 ounces at $0.10 per ounce ($1.00)

item_total = $0.00
-> 10 ounces at $0.25 per ounce ($2.50)
index = 1
item_total = item_total + $2.50
item_total = $0.00 + $2.50
item_total = $2.50
-> 12 ounces at $0.20 per ounce ($2.40)
index = 2
item_total = item_total + $2.40
item_total = $2.50 + $2.40
item_total = $4.90
-> 5 ounces at $0.30 per ounce ($1.50)
index = 0
item_total = item_total + ($1.50 * 50%)
item_total = $4.90 + $0.75
item_total = $5.65
-> 10 ounces at $0.10 per ounce ($1.00)
index = 1
item_total = item_total + $1.00
item_total = $5.65 + $1.00
item_total = $6.65

bulk_quantity = 1
sale_quantity = 2
discount_percentage = 50%

collected items:
5 ounces at $0.30 per ounce ($1.50)
10 ounces at $0.25 per ounce ($2.50)
10 ounces at $0.10 per ounce ($1.00)
12 ounces at $0.20 per ounce ($2.40)
12 ounces at $0.30 per ounce ($3.60)
10 ounces at $0.25 per ounce ($2.50)
10 ounces at $0.10 per ounce ($1.00)

sorted items:
12 ounces at $0.30 per ounce ($3.60)
10 ounces at $0.25 per ounce ($2.50)
10 ounces at $0.25 per ounce ($2.50)
12 ounces at $0.20 per ounce ($2.40)
5 ounces at $0.30 per ounce ($1.50)
10 ounces at $0.10 per ounce ($1.00)
10 ounces at $0.10 per ounce ($1.00)

item_total = $0.00
-> 12 ounces at $0.30 per ounce ($3.60)
index = 1
item_total = item_total + $3.60
item_total = $0.00 + $3.60
item_total = $3.60
-> 10 ounces at $0.25 per ounce ($2.50)
index = 2 (greater than bulk_quantity, discount applies)
item_total = item_total + ($2.50 * 50%)
item_total = $3.60 + $1.25
item_total = $4.85
-> 10 ounces at $0.25 per ounce ($2.50)
index = 0 (discount applies)
item_total = item_total + ($2.50 * 50%)
item_total = $4.85 + $1.25
item_total = $6.10
-> 12 ounces at $0.20 per ounce ($2.40)
index = 1
item_total = item_total + $2.40
item_total = $6.10 + $2.40
item_total = $8.50
-> 5 ounces at $0.30 per ounce ($1.50)
index = 2 (greater than bulk_quantity, discount applies)
item_total = item_total + ($1.50 * 50%)
item_total = $8.50 + $0.75
item_total = $9.25
-> 10 ounces at $0.10 per ounce ($1.00)
index = 0 (discount applies)
item_total = item_total + ($1.00 * 50%)
item_total = $9.25 + $0.50
item_total = $9.75
-> 10 ounces at $0.10 per ounce ($1.00)
index = 1
item_total = item_total + $1.00
item_total = $9.75 + $1.00
item_total = $10.75
```

## *As a customer, I want the ability to see the transaction total, so that I can be sure I am not overspending my budget.*

### Acceptance Criteria

**Given** a transaction
**And** all item totals have been calculated
**When** calculating the sale total
**Then** the sale total is the sum of all item totals.
