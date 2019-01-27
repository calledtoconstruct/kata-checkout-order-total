import { Transaction } from "../src/transaction";
import { LimitedUpSaleFlatPriceDiscount, Discount, DiscountListImplementation, DiscountList } from "../src/discount";
import { Priced, Item, StandardItem, ItemList, ItemListImplementation } from "../src/item";
import { Currency } from "../src/currency";
import { Parameterized, TestScenario } from "./parameterized";

export class TestLimitedUpSaleFlatPriceDiscount {

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

                        beforeEach(async (): Promise<void> => {
                            const itemList: ItemList = new ItemListImplementation();
                            itemList.add(item);
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
                                expect(itemTotal).toEqual(Currency.floor(quantity * price));
                            });

                        });

                        describe('And the right quantity', () => {

                            let itemTotal: number;
                            let quantity: number;

                            beforeEach(async (): Promise<void> => {
                                await transaction.scan(code);
                                await transaction.scan(code);
                                await transaction.scan(code);
                                itemTotal = await transaction.scan(code);
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

                            beforeEach(async (): Promise<void> => {
                                await transaction.scan(code);
                                await transaction.scan(code);
                                await transaction.scan(code);
                                await transaction.scan(code);
                                await transaction.scan(code);
                                await transaction.scan(code);
                                await transaction.scan(code);
                                await transaction.scan(code);
                                itemTotal = await transaction.scan(code);
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

        describe('Given a limited discount', () => {

            const bulk: number = 2;
            const sale: number = 1;

            const scenarios = new Parameterized<number, TestScenario<number>>([
                { description: 'of zero', target: 0 },
                { description: 'equal to the bulk quantity', target: bulk },
                { description: 'equal to the sale quantity', target: sale },
                { description: 'more than the sum of the bulk and sale quantities', target: bulk + sale + sale }
            ]);

            scenarios.forEach().describe('With a limit', (scenario: TestScenario<number>) => {

                const description: string = scenario.description;
                const limit: number = scenario.target;

                describe(description, () => {

                    const code: string = 'some by quantity item';
                    const today: number = new Date().valueOf();
                    const price: number = 1.00;
    
                    const discount: Discount = new LimitedUpSaleFlatPriceDiscount(
                        new Date(today - 10),
                        new Date(today + 10),
                        code,
                        bulk,
                        sale,
                        price,
                        limit
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
                            itemList.add(item);
                            try {
                                await discount.validate(itemList)
                            } catch (exception) {
                                error = exception;
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