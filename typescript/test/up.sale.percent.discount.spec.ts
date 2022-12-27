
import { Transaction } from "../src/transaction";
import { UpSalePercentDiscount, Discount, DiscountListImplementation, DiscountList } from "../src/discount";
import { ItemList, ItemListImplementation, Item, Priced, StandardItem } from "../src/item";
import { Currency } from "../src/currency";
import { Parameterized, TestScenario } from "./parameterized";

export class TestUpSalePercentDiscount {

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

                    const discount: Discount = new UpSalePercentDiscount(
                        new Date(today - 10),
                        new Date(today + 10),
                        code,
                        bulk,
                        sale,
                        discountPercent
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
                                expect(itemTotal).toEqual(Currency.floor(bulk * price + sale * (price * (1 - discountPercent))));
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
                                const salePrice = price * (1 - discountPercent);
                                expect(itemTotal).toEqual(Currency.floor(regularPriceQuantity * price + sale * salePrice));
                            });

                        });

                    });

                });

            });

        });

        describe('Given an up sale percent discount', () => {

            const scenarios = new Parameterized<number, TestScenario<number>>([
                { description: 'greater than one hundred', target: 1.20 },
                { description: 'equal to zero', target: 0 }
            ]);

            scenarios.forEach().describe('With a percent', (scenario: TestScenario<number>) => {

                const description: string = scenario.description;
                const discountPercent: number = scenario.target;

                describe(description, () => {

                    const code: string = 'some by quantity item';
                    const today: number = new Date().valueOf();
                    const bulk: number = 2;
                    const sale: number = 1;

                    const discount: Discount = new UpSalePercentDiscount(
                        new Date(today - 10),
                        new Date(today + 10),
                        code,
                        bulk,
                        sale,
                        discountPercent
                    );

                    describe('When validating', () => {

                        const price: number = 2.97;
                        const item: Item & Priced = new StandardItem(
                            code,
                            'random description',
                            'by quantity',
                            price
                        );

                        let error: Error | null = null;

                        beforeEach(async (): Promise<void> => {
                            const itemList: ItemList = new ItemListImplementation();
                            await itemList.add(item);
                            try {
                                await discount.validate(itemList)
                            } catch (exception) {
                                error = (exception instanceof Error)
                                    ? exception
                                    : new Error("Unexpected exception");
                            }
                        });

                        it('Should raise an error.', () => {
                            expect(error).not.toBeNull();
                        });

                    });

                });

            });

        });

    }

}
