import { Transaction } from "../src/transaction";
import { LimitedUpSaleFlatPriceDiscount, Discount, DiscountListImplementation, DiscountList } from "../src/discount";
import { Priced, Item, StandardItem, ItemList, ItemListImplementation } from "../src/item";
import { Currency } from "../src/currency";

export class TestLimitedUpSaleFlatPriceDiscount {

    public static Scenarios() {

        describe('Given a transaction', () => {

            describe('Given a transaction', () => {
    
                let transaction: Transaction;
    
                describe('And a by quantity item', () => {
    
                    const code: string = 'some by quantity item';
                    const price: number = 2.97;
                    const item: Item & Priced = new StandardItem(
                        code,
                        'random description',
                        'by quantity',
                        price
                    );
    
                    describe('And a discount rule for the same item', () => {
                        
                        const today: number = new Date().valueOf();
                        const bulk: number = 3;
                        const sale: number = 1;
                        const discountPrice: number = 1.00;
                        const limit: number = 8;
    
                        const discount: Discount = new LimitedUpSaleFlatPriceDiscount(
                            new Date(today - 10),
                            new Date(today + 10),
                            code,
                            bulk,
                            sale,
                            discountPrice,
                            limit
                        );
    
                        describe('When calculating', () => {
    
                            beforeEach(() => {
                                const itemList: ItemList = new ItemListImplementation();
                                itemList.add(item);
                                const discountList: DiscountList = new DiscountListImplementation(itemList);
                                discountList.add(discount);
                                transaction = new Transaction(itemList, discountList);
                            });
    
                            describe('And too few quantity', () => {
    
                                let itemTotal: number;
                                let quantity: number;
    
                                beforeEach(() => {
                                    transaction.scan(code);
                                    itemTotal = transaction.scan(code);
                                    quantity = transaction.quantity(code);
                                });
        
                                it('Then the item total should be the item quantity times the item price.', () => {
                                    expect(quantity).toBeLessThan(bulk + sale);
                                    expect(itemTotal).toEqual(Currency.floor(quantity * price));
                                });
        
                            });
        
                            describe('And the right quantity', () => {
    
                                let itemTotal: number;
                                let quantity: number;
    
                                beforeEach(() => {
                                    transaction.scan(code);
                                    transaction.scan(code);
                                    transaction.scan(code);
                                    itemTotal = transaction.scan(code);
                                    quantity = transaction.quantity(code);
                                });
        
                                it('Then the item total should the bulk quantity times the item price plus the sale quantity times the discount price.', () => {
                                    expect(quantity).toEqual(bulk + sale);
                                    expect(itemTotal).toEqual(Currency.floor(bulk * price + sale * discountPrice));
                                });
        
                            });
        
                            describe('And a quantity over the limit', () => {
    
                                let itemTotal: number;
                                let quantity: number;
    
                                beforeEach(() => {
                                    transaction.scan(code);
                                    transaction.scan(code);
                                    transaction.scan(code);
                                    transaction.scan(code);
                                    transaction.scan(code);
                                    transaction.scan(code);
                                    transaction.scan(code);
                                    transaction.scan(code);
                                    itemTotal = transaction.scan(code);
                                    quantity = transaction.quantity(code);
                                });
        
                                it('Then the item total should be calculated correctly.', () => {
                                    expect(quantity).toEqual(9);
                                    expect(itemTotal).toEqual(Currency.floor(7 * price + 2 * discountPrice));
                                });
        
                            });
    
                        });
    
                    });
    
                });
    
            });

        });

    }

}