
import { Transaction } from '../src/transaction';
import { BulkFlatPriceDiscount, Discount, DiscountListImplementation, DiscountList } from '../src/discount';
import { ItemList, ItemListImplementation, Item, Priced, StandardItem } from '../src/item';

export class TestBulkFlatPriceDiscount {

    public static scenarios() {

        describe('Given a transaction', () => {

            let transaction: Transaction;

            describe('and a by quantity item', () => {

                const code = 'some by quantity item';
                const price = 2.97;
                const item: Item & Priced = new StandardItem(
                    code,
                    'random description',
                    'by quantity',
                    price
                );
                const discountPrice = 5.00;
                const today: number = new Date().valueOf();
                const bulk = 3;

                describe('and a bulk flat price discount for the same item', () => {

                    const discount: Discount = new BulkFlatPriceDiscount(
                        new Date(today - 10),
                        new Date(today + 10),
                        code,
                        bulk,
                        discountPrice
                    );

                    beforeEach(async (): Promise<void> => {
                        const itemList: ItemList = new ItemListImplementation();
                        await itemList.add(item);
                        const discountList: DiscountList = new DiscountListImplementation(itemList);
                        await discountList.add(discount);
                        transaction = new Transaction(itemList, discountList);
                    });

                    describe('When calculating', () => {

                        describe('too few items', () => {

                            let itemTotal: number;
                            let quantity: number;

                            beforeEach(async (): Promise<void> => {
                                await transaction.scan(code);
                                itemTotal = await transaction.scan(code);
                                quantity = transaction.quantity(code);
                            });

                            it('Then the item total should be the quantity times the item price.', () => {
                                expect(quantity).toBeLessThan(bulk);
                                expect(itemTotal).toEqual(quantity * price);
                            });

                        });

                        describe('the right number of items', () => {

                            let itemTotal: number;
                            let quantity: number;

                            beforeEach(async (): Promise<void> => {
                                await transaction.scan(code);
                                await transaction.scan(code);
                                itemTotal = await transaction.scan(code);
                                quantity = transaction.quantity(code);
                            });

                            it('Then the item total should be the quantity times the discount price.', () => {
                                expect(quantity).toEqual(bulk);
                                expect(itemTotal).toEqual(quantity * discountPrice);
                            });

                        });

                        describe('more than enough items', () => {

                            let itemTotal: number;
                            let quantity: number;

                            beforeEach(async (): Promise<void> => {
                                await transaction.scan(code);
                                await transaction.scan(code);
                                await transaction.scan(code);
                                await transaction.scan(code);
                                itemTotal = await transaction.scan(code);
                                quantity = transaction.quantity(code);
                            });

                            it('Then the item total should be the quantity times the discount price.', () => {
                                expect(quantity).toBeGreaterThan(bulk);
                                expect(itemTotal).toEqual((bulk * discountPrice) + ((quantity - bulk) * price));
                            });

                        });

                    });

                });

            });

        });

    }
}
