
import { Transaction } from "../src/transaction";
import { Priced, Item, StandardItem, ItemList, ItemListImplementation } from "../src/item";
import { Discount, DiscountListImplementation, DiscountList, UpSaleFlatPriceDiscount } from "../src/discount";

export class TestUpSaleFlatPriceDiscount {

    public static scenarios() {

        describe('Given a transaction', () => {

            let transaction: Transaction;

            describe('And a by quantity item', () => {

                const code: string = 'some by quantity item';
                const price: number = 19.99;
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
                    const discountPrice: number = 1.00;

                    const discount: Discount = new UpSaleFlatPriceDiscount(
                        new Date(today - 10),
                        new Date(today + 10),
                        code,
                        bulk,
                        sale,
                        discountPrice
                    );

                    describe('When calculating', () => {

                        beforeEach(async (): Promise<void> => {
                            const itemList: ItemList = new ItemListImplementation();
                            await itemList.add(item);
                            const discountList: DiscountList = new DiscountListImplementation(itemList);
                            await discountList.add(discount);
                            transaction = new Transaction(itemList, discountList);
                        });

                        describe('And too few quantity', () => {

                            let itemTotal: number;
                            let quantity: number;

                            beforeEach(async (): Promise<void> => {
                                await transaction.scan(code);
                                itemTotal = await transaction.scan(code);
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

                            beforeEach(async (): Promise<void> => {
                                await transaction.scan(code);
                                await transaction.scan(code);
                                itemTotal = await transaction.scan(code);
                                quantity = transaction.quantity(code);
                            });

                            it('Then the item total should the bulk quantity times the item price plus the sale quantity times the discount price.', () => {
                                expect(quantity).toEqual(bulk + sale);
                                expect(itemTotal).toEqual(bulk * price + sale * (price * discountPrice));
                            });

                        });

                        describe('And too high a quantity', () => {

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

                            it('Then the item total should be calculated correctly.', () => {
                                expect(quantity).toBeGreaterThan(bulk + sale);
                                const regularPriceQuantity = quantity - sale;
                                const salePrice = price * discountPrice;
                                expect(itemTotal).toEqual(regularPriceQuantity * price + sale * salePrice);
                            });

                        });

                    });

                });

            });

        });

    }

}
