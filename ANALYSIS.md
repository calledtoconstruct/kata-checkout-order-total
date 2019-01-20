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

> buy 2 get 1 for $1.00

**Given** a collection of pricing rules
**And** a new rule containing a start date, end date, item code, bulk quantity, sale quantity, and price
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

**Given** a collection of pricing rules
**And** an invalid rule (not matching the above rule types)
**When** adding the rule
**Then** and error is raised.


# Epic - Scanning

## *As a cashier, I want the ability to scan an item, so that items can be easily added to the transaction.*

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

## *As a customer, I want to see the discounted price for sale items, so that I can be sure I am getting the advertised sale price.*

## *As a customer, I want the ability to see the transaction total, so that I can be sure I am not overspending my budget.*
