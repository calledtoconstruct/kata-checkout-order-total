# Process Flow

# Customer

```
Bring cart of items to cashier
```

# Cashier

```
Start Transaction
-> Transaction API (start) -> Transaction ID

for each item
    Scan Item
    -> Transaction API (add item) -> Total | Error
    -> Display Total

for each mistake
    Void Item
    -> Transaction API (remove item) -> Total | Error
    -> Display Total
```

# Transaction API

```
Start
-> Create Transaction
-> Return Transaction ID

Add Item
-> (async) Item API (get item) -> Item | Error
    -> Add Item | Return Error
-> (async) Discount API (get item discount) -> Item Discount | Nothing
    -> Add Item Discount | Do Nothing
-> (sync)
-> Calculate Total
-> Return Total

Remove Item
-> Find Item | Return Error
-> Remove Item
-> Calculate Total
-> Return Total

Calculate Total
-> Set Total = 0
-> for each item
    -> if not already included
        -> Get Calculation Strategy
        -> Set Total += Calculation Strategy (item)
-> Return Total

Get Calculation Strategy
-> if discount applies
    -> Return Discount Strategy
-> otherwise
    -> if by quantity
        -> Return Quantity * Price Strategy
    -> otherwise
        -> Return Weight * Price Strategy

```

# Item API

```
(on start)
-> Load Items

Get Item
-> Find Item | Return Error
-> Return Item

Load Items
-> Open File
-> for each item
    -> Verify Uniqueness by Item Code | Error
    -> Add Item
```

# Discount API

```
(on start)
-> Load Item Discounts

Get Item Discount
-> Filter By Item Code and Date Range
-> Return Single | Nothing

Load Item Discounts
-> Open File
-> for each item
    -> Verify Uniqueness by Item Code and Date Range | Error
    -> Add Item
```
