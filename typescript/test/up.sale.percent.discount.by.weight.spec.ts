
import { Transaction } from '../src/transaction';
import { Priced, Item, StandardItem, ItemList, ItemListImplementation } from '../src/item';
import { UpSalePercentDiscountByWeight, Discount, DiscountListImplementation, DiscountList } from '../src/discount';
import { Currency } from '../src/currency';
import { Parameterized, TestScenario } from './parameterized';

export class TestUpSalePercentDiscountByWeight {

    public static scenarios() {

        describe('Given a transaction', () => {

            let transaction: Transaction;

            describe('And a by quantity item', () => {

                const code = 'some by quantity item';
                const price = 2.97;
                const item: Item & Priced = new StandardItem(
                    code,
                    'random description',
                    'by weight',
                    price
                );

                describe('And a discount rule for the same item', () => {

                    const today: number = new Date().valueOf();
                    const bulk = 2;
                    const sale = 1;
                    const discountPercent = 0.50;

                    const discount: Discount = new UpSalePercentDiscountByWeight(
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

                            const firstWeight = 2.5;
                            const secondWeight = 3.25;

                            let itemTotal: number;
                            let quantity: number;

                            beforeEach(async (): Promise<void> => {
                                await transaction.scan(code, firstWeight);
                                itemTotal = await transaction.scan(code, secondWeight);
                                quantity = transaction.quantity(code);
                            });

                            it('Then the item total should be the item quantity times the item price.', () => {
                                expect(quantity).toBeLessThan(bulk + sale);
                                expect(itemTotal).toEqual(Currency.floor((firstWeight + secondWeight) * price));
                            });

                        });

                        describe('And the right quantity', () => {

                            const firstWeight = 2.5;
                            const secondWeight = 3.25;
                            const thirdWeight = 4.00;

                            let itemTotal: number;
                            let quantity: number;

                            beforeEach(async (): Promise<void> => {
                                await transaction.scan(code, firstWeight);
                                await transaction.scan(code, secondWeight);
                                itemTotal = await transaction.scan(code, thirdWeight);
                                quantity = transaction.quantity(code);
                            });

                            it('Then the item total should the bulk quantity times the item price plus the sale quantity times the discount price.', () => {
                                expect(quantity).toEqual(bulk + sale);
                                const fullPrice: number = (secondWeight + thirdWeight) * price;
                                const salePrice: number = firstWeight * price * (1 - discountPercent);
                                expect(itemTotal).toEqual(Currency.floor(fullPrice + salePrice));
                            });

                        });

                        describe('And more than enough items', () => {

                            const firstWeight = 2.5;
                            const secondWeight = 3.25;
                            const thirdWeight = 4.00;
                            const fourthWeight = 1.5;
                            const fifthWeight = 5.25;
                            const sixthWeight = 4.00;
                            const seventhWeight = 1.0;
                            const eighthWeight = 3.25;

                            let itemTotal: number;
                            let quantity: number;

                            beforeEach(async (): Promise<void> => {
                                await transaction.scan(code, firstWeight);
                                await transaction.scan(code, secondWeight);
                                await transaction.scan(code, thirdWeight);
                                await transaction.scan(code, fourthWeight);
                                await transaction.scan(code, fifthWeight);
                                await transaction.scan(code, sixthWeight);
                                await transaction.scan(code, seventhWeight);
                                itemTotal = await transaction.scan(code, eighthWeight);
                                quantity = transaction.quantity(code);
                            });

                            it('Then the item total should be calculated correctly.', () => {
                                expect(quantity).toEqual(8);
                                const fullPrice: number = (fifthWeight + sixthWeight + secondWeight + eighthWeight + fourthWeight + seventhWeight) * price;
                                const salePrice: number = (thirdWeight + firstWeight) * price * (1 - discountPercent);
                                expect(itemTotal).toEqual(Currency.floor(fullPrice + salePrice));
                            });

                        });

                    });

                });

            });

        });

        describe('Given an up sale percent discount by weight', () => {

            const scenarios = new Parameterized<number, TestScenario<number>>([
                { description: 'greater than one hundred', target: 1.20 },
                { description: 'equal to zero', target: 0 }
            ]);

            scenarios.forEach().describe('With a percent', (scenario: TestScenario<number>) => {

                const description: string = scenario.description;
                const discountPercent: number = scenario.target;

                describe(description, () => {

                    const code = 'some by weight item';
                    const today: number = new Date().valueOf();
                    const bulk = 2;
                    const sale = 1;

                    const discount: Discount = new UpSalePercentDiscountByWeight(
                        new Date(today - 10),
                        new Date(today + 10),
                        code,
                        bulk,
                        sale,
                        discountPercent
                    );

                    describe('When validating', () => {

                        const price = 2.97;
                        const item: Item & Priced = new StandardItem(
                            code,
                            'random description',
                            'by weight',
                            price
                        );

                        let error: Error | null = null;

                        beforeEach(async (): Promise<void> => {
                            const itemList: ItemList = new ItemListImplementation();
                            await itemList.add(item);
                            try {
                                await discount.validate(itemList);
                            } catch (exception) {
                                error = (exception instanceof Error)
                                    ? exception
                                    : new Error('Unexpected exception');
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
