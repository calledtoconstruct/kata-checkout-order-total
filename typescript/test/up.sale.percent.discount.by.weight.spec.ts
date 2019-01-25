import { Transaction } from "../src/transaction";
import { Priced, Item, StandardItem, ItemList, ItemListImplementation } from "../src/item";
import { UpSalePercentDiscountByWeight, Discount, DiscountListImplementation, DiscountList } from "../src/discount";
import { Currency } from "../src/currency";

export class TestUpSalePercentDiscountByWeight {

    public static scenarios() {

        describe('Given a transaction', () => {

            let transaction: Transaction;

            describe('And a by quantity item', () => {

                const code: string = 'some by quantity item';
                const price: number = 2.97;
                const item: Item & Priced = new StandardItem(
                    code,
                    'random description',
                    'by weight',
                    price
                );

                describe('And a discount rule for the same item', () => {

                    const today: number = new Date().valueOf();
                    const bulk: number = 2;
                    const sale: number = 1;
                    const discountPercent: number = 0.50;

                    const discount: Discount = new UpSalePercentDiscountByWeight(
                        new Date(today - 10),
                        new Date(today + 10),
                        code,
                        bulk,
                        sale,
                        discountPercent
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

                            const firstWeight: number = 2.5;
                            const secondWeight: number = 3.25;

                            let itemTotal: number;
                            let quantity: number;

                            beforeEach(() => {
                                transaction.scan(code, firstWeight);
                                itemTotal = transaction.scan(code, secondWeight);
                                quantity = transaction.quantity(code);
                            });

                            it('Then the item total should be the item quantity times the item price.', () => {
                                expect(quantity).toBeLessThan(bulk + sale);
                                expect(itemTotal).toEqual(Currency.floor((firstWeight + secondWeight) * price));
                            });

                        });

                        describe('And the right quantity', () => {

                            const firstWeight: number = 2.5;
                            const secondWeight: number = 3.25;
                            const thirdWeight: number = 4.00;

                            let itemTotal: number;
                            let quantity: number;

                            beforeEach(() => {
                                transaction.scan(code, firstWeight);
                                transaction.scan(code, secondWeight);
                                itemTotal = transaction.scan(code, thirdWeight);
                                quantity = transaction.quantity(code);
                            });

                            it('Then the item total should the bulk quantity times the item price plus the sale quantity times the discount price.', () => {
                                expect(quantity).toEqual(bulk + sale);
                                const fullPrice: number = (secondWeight + thirdWeight) * price;
                                const salePrice: number = firstWeight * price * (1 - discountPercent);
                                expect(itemTotal).toEqual(Currency.floor(fullPrice + salePrice));
                            });

                        });

                    });

                });

            });

        });

    }

}