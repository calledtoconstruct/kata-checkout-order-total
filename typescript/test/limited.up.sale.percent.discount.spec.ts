import { Transaction } from "../src/transaction";
import { Priced, Item, StandardItem, ItemList, ItemListImplementation } from "../src/item";
import { Discount, LimitedUpSalePercentDiscount, DiscountListImplementation, DiscountList } from "../src/discount";
import { Currency } from "../src/currency";

export class TestLimitedUpSalePercentDiscount {

    public static scenarios() {

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
                    const bulk: number = 2;
                    const sale: number = 1;
                    const discountPercent: number = 0.20;
                    const limit: number = 6;

                    const discount: Discount = new LimitedUpSalePercentDiscount(
                        new Date(today - 10),
                        new Date(today + 10),
                        code,
                        bulk,
                        sale,
                        discountPercent,
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
                                expect(itemTotal).toEqual(quantity * price);
                            });
    
                        });
    
                        describe('And the right quantity', () => {

                            let itemTotal: number;
                            let quantity: number;

                            beforeEach(() => {
                                transaction.scan(code);
                                transaction.scan(code);
                                itemTotal = transaction.scan(code);
                                quantity = transaction.quantity(code);
                            });
    
                            it('Then the item total should the bulk quantity times the item price plus the sale quantity times the discount price.', () => {
                                expect(quantity).toEqual(bulk + sale);
                                expect(itemTotal).toEqual(Currency.floor(bulk * price + sale * (price * (1 - discountPercent))));
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
                                const salePrice = price * (1 - discountPercent);
                                expect(itemTotal).toEqual(Currency.floor(7 * price + 2 * salePrice));
                            });
    
                        });

                    });

                });

            });

        });

    }

}
