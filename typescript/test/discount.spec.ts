
import { Parameterized, TestScenario } from './parameterized';

import { ItemList, Item, Priced, ItemType } from '../src/item';

import {
    Discount,
    StandardDiscount,
    DiscountList,
    BulkFlatPriceDiscount,
    UpSalePercentDiscount,
    LimitedUpSalePercentDiscount,
    UpSaleFlatPriceDiscount,
    LimitedUpSaleFlatPriceDiscount,
    UpSalePercentDiscountByWeight,
    DiscountListImplementation
} from '../src/discount';

import { DateRange } from '../src/date';

class FakeItem implements Item {
    public readonly code: string = '';
    public readonly type: ItemType;
    public validate(): void { }
}

class FakeByQuantityItem implements Item {
    public readonly code: string = 'by quantity item';
    public readonly type: ItemType = 'by quantity';
    public validate(): void { }
}

class FakeByWeightItem implements Item {
    public readonly code: string = 'by weight item';
    public readonly type: ItemType = 'by weight';
    public validate(): void { }
}

class FakeItemList implements ItemList {
    private readonly items: any = {};
    public add(_: Item): void {
        throw new Error("Method not implemented.");
    }
    public includes(_: Item): boolean {
        throw new Error("Method not implemented.");
    }
    public get(code: string): Item & Priced {
        if (this.items[code] === undefined) {
            throw new Error('invalid test');
        }
        return this.items[code];
    }
    public addFakeItem(code: string, item: Item): void {
        this.items[code] = item;
    }
}

export class TestDiscountList {

    public static Scenarios(): void {

        describe('Given a collection of Pricing Rules', () => {

            let discountList: DiscountList;

            beforeEach(() => {
                const fakeItemList: FakeItemList = new FakeItemList();
                const itemList: ItemList = fakeItemList;

                fakeItemList.addFakeItem('by quantity item', new FakeByQuantityItem());
                fakeItemList.addFakeItem('by weight item', new FakeByWeightItem());

                discountList = new DiscountListImplementation(itemList);
            });

            const discountMonth: DateRange = {
                startDate: new Date(2010, 3, 1, 0, 0, 0, 0),
                endDate: new Date(2010, 3, 31, 11, 59, 59, 999)
            };

            const validDiscountScenarios: Parameterized<Discount, TestScenario<Discount>> = new Parameterized<Discount, TestScenario<Discount>>([
                { description: 'standard discount', target: new StandardDiscount(discountMonth.startDate, discountMonth.endDate, 'by quantity item', 1.0) },
                { description: 'bulk flat price', target: new BulkFlatPriceDiscount(discountMonth.startDate, discountMonth.endDate, 'by quantity item', 3, 5.0) },
                { description: 'up sale percent discount', target: new UpSalePercentDiscount(discountMonth.startDate, discountMonth.endDate, 'by quantity item', 2, 1, 0.5) },
                { description: 'limited up sale percent discount', target: new LimitedUpSalePercentDiscount(discountMonth.startDate, discountMonth.endDate, 'by quantity item', 3, 1, 1, 8) },
                { description: 'up sale flat price discount', target: new UpSaleFlatPriceDiscount(discountMonth.startDate, discountMonth.endDate, 'by quantity item', 2, 1, 1.25) },
                { description: 'limited up sale flat price discount', target: new LimitedUpSaleFlatPriceDiscount(discountMonth.startDate, discountMonth.endDate, 'by quantity item', 3, 2, 1, 10) },
                { description: 'up sale percent discount by weight', target: new UpSalePercentDiscountByWeight(discountMonth.startDate, discountMonth.endDate, 'by weight item', 2, 1, .5) }
            ]);

            validDiscountScenarios.forEach().describe('And a valid discount When adding it', (discountScenario: TestScenario<Discount>) => {

                const scenarioDescription: string = discountScenario.description;
                const discount: Discount = discountScenario.target;

                beforeEach(() => {
                    discountList.add(discount);
                });

                describe(scenarioDescription, () => {

                    it('Should be added to the list', () => {
                        const result = discountList.includes(discount);
                        expect(result).toBe(true);
                    });

                });

            });

            const invalidDiscountsWithItemTypeMismatch: Parameterized<Discount, TestScenario<Discount>> = new Parameterized<Discount, TestScenario<Discount>>([
                { description: 'standard discount item type mismatch', target: new StandardDiscount(new Date(), new Date(), 'by weight item', 1.0) },
                { description: 'bulk flat price item type mismatch', target: new BulkFlatPriceDiscount(new Date(), new Date(), 'by weight item', 3, 5.0) },
                { description: 'up sale percent discount item type mismatch', target: new UpSalePercentDiscount(new Date(), new Date(), 'by weight item', 2, 1, 0.5) },
                { description: 'limited up sale percent discount item type mismatch', target: new LimitedUpSalePercentDiscount(new Date(), new Date(), 'by weight item', 3, 1, 1, 8) },
                { description: 'up sale flat price discount item type mismatch', target: new UpSaleFlatPriceDiscount(new Date(), new Date(), 'by weight item', 2, 1, 1.25) },
                { description: 'limited up sale flat price discount item type mismatch', target: new LimitedUpSaleFlatPriceDiscount(new Date(), new Date(), 'by weight item', 3, 2, 1, 10) },
                { description: 'up sale percent discount by weight item type mismatch', target: new UpSalePercentDiscountByWeight(new Date(), new Date(), 'by quantity item', 2, 1, .5) },
                { description: 'standard discount no matching item', target: new StandardDiscount(new Date(), new Date(), 'no matching item', 1.0) },
                { description: 'bulk flat price no matching item', target: new BulkFlatPriceDiscount(new Date(), new Date(), 'no matching item', 3, 5.0) },
                { description: 'up sale percent discount no matching item', target: new UpSalePercentDiscount(new Date(), new Date(), 'no matching item', 2, 1, 0.5) },
                { description: 'limited up sale percent discount no matching item', target: new LimitedUpSalePercentDiscount(new Date(), new Date(), 'no matching item', 3, 1, 1, 8) },
                { description: 'up sale flat price discount no matching item', target: new UpSaleFlatPriceDiscount(new Date(), new Date(), 'no matching item', 2, 1, 1.25) },
                { description: 'limited up sale flat price discount no matching item', target: new LimitedUpSaleFlatPriceDiscount(new Date(), new Date(), 'no matching item', 3, 2, 1, 10) },
                { description: 'up sale percent discount by weight no matching item', target: new UpSalePercentDiscountByWeight(new Date(), new Date(), 'no matching item', 2, 1, .5) }
            ]);

            invalidDiscountsWithItemTypeMismatch.forEach().describe('And an invalid discount When adding it', (typeMismatchScenario: TestScenario<Discount>) => {

                const scenarioDescription: string = typeMismatchScenario.description;
                const discount: Discount = typeMismatchScenario.target;
                let error: Error | null = null;

                beforeEach(() => {
                    try {
                        discountList.add(discount);
                    } catch (exception) {
                        error = exception;
                    }
                });

                describe(scenarioDescription, () => {

                    it('Should raise an error', () => {
                        expect(error).not.toBeNull();
                    });

                    it('Should not be added to the list', () => {
                        const result = discountList.includes(discount);
                        expect(result).toBe(false);
                    });

                });

            });

            const weekInsideMonth: DateRange = {
                startDate: new Date(2010, 3, 5, 0, 0, 0, 0),
                endDate: new Date(2010, 3, 11, 11, 59, 59, 999)
            };

            const weekOverlappingBeginningOfMonth: DateRange = {
                startDate: new Date(2010, 2, 27, 0, 0, 0, 0),
                endDate: new Date(2010, 3, 5, 11, 59, 59, 999)
            };

            const weekOverlappingEndOfMonth: DateRange = {
                startDate: new Date(2010, 3, 28, 0, 0, 0, 0),
                endDate: new Date(2010, 4, 7, 11, 59, 59, 999)
            };

            const quarterOverlappingEntireMonth: DateRange = {
                startDate: new Date(2010, 1, 3, 0, 0, 0, 0),
                endDate: new Date(2010, 4, 3, 11, 59, 59, 999)
            };

            const overlappingDateScenarios: Parameterized<DateRange, TestScenario<DateRange>> = new Parameterized<DateRange, TestScenario<DateRange>>([
                { description: 'week inside of month', target: weekInsideMonth },
                { description: 'week overlapping beginning of month', target: weekOverlappingBeginningOfMonth },
                { description: 'week overlapping end of month', target: weekOverlappingEndOfMonth },
                { description: 'quarter overlapping entire month', target: quarterOverlappingEntireMonth }
            ]);

            overlappingDateScenarios.forEach().describe('When adding a duplicate', (overlappingDateScenario: TestScenario<DateRange>) => {

                const scenarioDescription: string = overlappingDateScenario.description;
                const overlappingDateRange: DateRange = overlappingDateScenario.target;

                const overlappingDiscountScenarios: Parameterized<Discount, TestScenario<Discount>> = new Parameterized<Discount, TestScenario<Discount>>([
                    { description: 'standard discount', target: new StandardDiscount(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by quantity item', 1.0) },
                    { description: 'bulk flat price', target: new BulkFlatPriceDiscount(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by quantity item', 3, 5.0) },
                    { description: 'up sale percent discount', target: new UpSalePercentDiscount(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by quantity item', 2, 1, 0.5) },
                    { description: 'limited up sale percent discount', target: new LimitedUpSalePercentDiscount(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by quantity item', 3, 1, 1, 8) },
                    { description: 'up sale flat price discount', target: new UpSaleFlatPriceDiscount(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by quantity item', 2, 1, 1.25) },
                    { description: 'limited up sale flat price discount', target: new LimitedUpSaleFlatPriceDiscount(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by quantity item', 3, 2, 1, 10) },
                    { description: 'up sale percent discount by weight', target: new UpSalePercentDiscountByWeight(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by weight item', 2, 1, .5) }
                ]);

                overlappingDiscountScenarios.forEach().describe(scenarioDescription, (overlappingDiscountScenario: TestScenario<Discount>) => {

                    const scenarioDescription: string = overlappingDiscountScenario.description;
                    const overlappingDiscount: Discount = overlappingDiscountScenario.target;
                    const whenSameCode = (validDiscountScenario: TestScenario<Discount>): boolean => validDiscountScenario.target.code === overlappingDiscount.code;

                    validDiscountScenarios.forEach(whenSameCode).describe(scenarioDescription, (discountScenario: TestScenario<Discount>) => {

                        const scenarioDescription: string = discountScenario.description;
                        const discount: Discount = discountScenario.target;
                        let error: Error | null = null;

                        beforeEach(() => {
                            discountList.add(discount);
                            try {
                                discountList.add(overlappingDiscount);
                            } catch (exception) {
                                error = exception;
                            }
                        });

                        describe(scenarioDescription, () => {

                            it('Should raise an error.', () => {
                                expect(error).not.toBeNull();
                            });

                            it('Should not be included.', () => {
                                const result = discountList.includes(overlappingDiscount);
                                expect(result).not.toBe(true)
                            });

                            it('Should not replace existing.', () => {
                                const result = discountList.includes(discount);
                                expect(result).toBe(true);
                            });

                        });

                    });

                });

            });

        });

        const invertedDateRange: DateRange = {
            startDate: new Date(2010, 3, 31, 0, 0, 0, 0),
            endDate: new Date(2010, 3, 1, 11, 59, 59, 999)
        };

        const invertedDateRangeDiscountScenarios: Parameterized<Discount, TestScenario<Discount>> = new Parameterized<Discount, TestScenario<Discount>>([
            { description: 'standard discount', target: new StandardDiscount(invertedDateRange.startDate, invertedDateRange.endDate, 'by quantity item', 1.0) },
            { description: 'bulk flat price', target: new BulkFlatPriceDiscount(invertedDateRange.startDate, invertedDateRange.endDate, 'by quantity item', 3, 5.0) },
            { description: 'up sale percent discount', target: new UpSalePercentDiscount(invertedDateRange.startDate, invertedDateRange.endDate, 'by quantity item', 2, 1, 0.5) },
            { description: 'limited up sale percent discount', target: new LimitedUpSalePercentDiscount(invertedDateRange.startDate, invertedDateRange.endDate, 'by quantity item', 3, 1, 1, 8) },
            { description: 'up sale flat price discount', target: new UpSaleFlatPriceDiscount(invertedDateRange.startDate, invertedDateRange.endDate, 'by quantity item', 2, 1, 1.25) },
            { description: 'limited up sale flat price discount', target: new LimitedUpSaleFlatPriceDiscount(invertedDateRange.startDate, invertedDateRange.endDate, 'by quantity item', 3, 2, 1, 10) },
            { description: 'up sale percent discount by weight', target: new UpSalePercentDiscountByWeight(invertedDateRange.startDate, invertedDateRange.endDate, 'by weight item', 2, 1, .5) }
        ]);

        invertedDateRangeDiscountScenarios.forEach().describe('Given a pricing rule with an inverted date range', (invertedDateRangeDiscountScenario: TestScenario<Discount>) => {

            const scenarioDescription: string = invertedDateRangeDiscountScenario.description;
            const discount: Discount = invertedDateRangeDiscountScenario.target;
            const fakeItemList: FakeItemList = new FakeItemList();
            const itemList: ItemList = fakeItemList;

            describe(scenarioDescription, () => {

                fakeItemList.addFakeItem('by quantity item', new FakeByQuantityItem());
                fakeItemList.addFakeItem('by weight item', new FakeByWeightItem());

                describe('When validating', () => {
                    let error: Error | null = null;

                    beforeEach(() => {
                        try {
                            discount.validate(itemList);
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

    }

}

export class TestDiscount {

    public static Scenarios(): void {

        const invalidItemCodeDiscountScenarios: Parameterized<Discount, TestScenario<Discount>> = new Parameterized<Discount, TestScenario<Discount>>([
            { description: 'standard discount with empty item code', target: new StandardDiscount(new Date(), new Date(), '', 1.0) },
            { description: 'bulk flat price with empty item code', target: new BulkFlatPriceDiscount(new Date(), new Date(), '', 3, 5.0) },
            { description: 'up sale percent discount with empty item code', target: new UpSalePercentDiscount(new Date(), new Date(), '', 2, 1, 0.5) },
            { description: 'limited up sale percent discount with empty item code', target: new LimitedUpSalePercentDiscount(new Date(), new Date(), '', 3, 1, 1, 8) },
            { description: 'up sale flat price discount with empty item code', target: new UpSaleFlatPriceDiscount(new Date(), new Date(), '', 2, 1, 1.25) },
            { description: 'limited up sale flat price discount with empty item code', target: new LimitedUpSaleFlatPriceDiscount(new Date(), new Date(), '', 3, 2, 1, 10) },
            { description: 'up sale percent discount by weight with empty item code', target: new UpSalePercentDiscountByWeight(new Date(), new Date(), '', 2, 1, .5) }
        ]);

        invalidItemCodeDiscountScenarios.forEach().describe('Given a', (invalidDiscountScenario: TestScenario<Discount>) => {

            const scenarioDescription: string = invalidDiscountScenario.description;
            const invalidDiscount: Discount = invalidDiscountScenario.target;
            const fakeItemList: FakeItemList = new FakeItemList();
            const itemList: ItemList = fakeItemList;
            const expectedError: Error = new Error('Invalid Item Code');

            describe(scenarioDescription, () => {

                fakeItemList.addFakeItem('', new FakeItem());

                describe('When validating', () => {
                    let error: Error | null = null;

                    beforeEach(() => {
                        try {
                            invalidDiscount.validate(itemList);
                        } catch (exception) {
                            error = exception;
                        }
                    });

                    it('Should raise an error.', () => {
                        expect(error).not.toBeNull();
                        expect(error).toEqual(expectedError);
                    });

                });

            });

        });

        const invalidBulkQuantityDiscountScenarios: Parameterized<Discount, TestScenario<Discount>> = new Parameterized<Discount, TestScenario<Discount>>([
            { description: 'up sale percent discount with zero bulk quantity', target: new UpSalePercentDiscount(new Date(), new Date(), 'by quantity item', 0, 1, 0.5) },
            { description: 'limited up sale percent discount with zero bulk quantity', target: new LimitedUpSalePercentDiscount(new Date(), new Date(), 'by quantity item', 0, 1, 1, 8) },
            { description: 'up sale flat price discount with zero bulk quantity', target: new UpSaleFlatPriceDiscount(new Date(), new Date(), 'by quantity item', 0, 1, 1.25) },
            { description: 'limited up sale flat price discount with zero bulk quantity', target: new LimitedUpSaleFlatPriceDiscount(new Date(), new Date(), 'by quantity item', 0, 2, 1, 10) },
            { description: 'up sale percent discount by weight with zero bulk quantity', target: new UpSalePercentDiscountByWeight(new Date(), new Date(), 'by weight item', 0, 1, .5) }
        ]);

        invalidBulkQuantityDiscountScenarios.forEach().describe('Given a', (invalidDiscountScenario: TestScenario<Discount>) => {

            const scenarioDescription: string = invalidDiscountScenario.description;
            const invalidDiscount: Discount = invalidDiscountScenario.target;
            const fakeItemList: FakeItemList = new FakeItemList();
            const itemList: ItemList = fakeItemList;
            const expectedError: Error = new Error('Bulk Quantity must be Greater Than Zero');

            describe(scenarioDescription, () => {

                fakeItemList.addFakeItem('by quantity item', new FakeByQuantityItem());
                fakeItemList.addFakeItem('by weight item', new FakeByWeightItem());

                describe('When validating', () => {
                    let error: Error | null = null;

                    beforeEach(() => {
                        try {
                            invalidDiscount.validate(itemList);
                        } catch (exception) {
                            error = exception;
                        }
                    });

                    it('Should raise an error.', () => {
                        expect(error).not.toBeNull();
                        expect(error).toEqual(expectedError);
                    });

                });

            });

        });

        const invalidSaleQuantityDiscountScenarios: Parameterized<Discount, TestScenario<Discount>> = new Parameterized<Discount, TestScenario<Discount>>([
            { description: 'up sale percent discount with zero sale quantity', target: new UpSalePercentDiscount(new Date(), new Date(), 'by quantity item', 2, 0, 0.5) },
            { description: 'limited up sale percent discount with zero sale quantity', target: new LimitedUpSalePercentDiscount(new Date(), new Date(), 'by quantity item', 3, 0, 1, 8) },
            { description: 'up sale flat price discount with zero sale quantity', target: new UpSaleFlatPriceDiscount(new Date(), new Date(), 'by quantity item', 2, 0, 1.25) },
            { description: 'limited up sale flat price discount with zero sale quantity', target: new LimitedUpSaleFlatPriceDiscount(new Date(), new Date(), 'by quantity item', 3, 0, 1, 10) },
            { description: 'up sale percent discount by weight with zero sale quantity', target: new UpSalePercentDiscountByWeight(new Date(), new Date(), 'by weight item', 5, 0, .5) }
        ]);

        invalidSaleQuantityDiscountScenarios.forEach().describe('Given a', (invalidDiscountScenario: TestScenario<Discount>) => {

            const scenarioDescription: string = invalidDiscountScenario.description;
            const invalidDiscount: Discount = invalidDiscountScenario.target;
            const fakeItemList: FakeItemList = new FakeItemList();
            const itemList: ItemList = fakeItemList;
            const expectedError: Error = new Error('Sale Quantity must be Greater Than Zero');

            describe(scenarioDescription, () => {

                fakeItemList.addFakeItem('by quantity item', new FakeByQuantityItem());
                fakeItemList.addFakeItem('by weight item', new FakeByWeightItem());

                describe('When validating', () => {
                    let error: Error | null = null;

                    beforeEach(() => {
                        try {
                            invalidDiscount.validate(itemList);
                        } catch (exception) {
                            error = exception;
                        }
                    });

                    it('Should raise an error.', () => {
                        expect(error).not.toBeNull();
                        expect(error).toEqual(expectedError);
                    });

                });

            });

        });

        const invalidFractionalBulkQuantityDiscountScenarios: Parameterized<Discount, TestScenario<Discount>> = new Parameterized<Discount, TestScenario<Discount>>([
            { description: 'up sale percent discount with fractional bulk quantity', target: new UpSalePercentDiscount(new Date(), new Date(), 'by quantity item', 2.5, 1, 0.5) },
            { description: 'limited up sale percent discount with fractional bulk quantity', target: new LimitedUpSalePercentDiscount(new Date(), new Date(), 'by quantity item', 0.25, 1, 1, 8) },
            { description: 'up sale flat price discount with fractional bulk quantity', target: new UpSaleFlatPriceDiscount(new Date(), new Date(), 'by quantity item', 1.111, 1, 1.25) },
            { description: 'limited up sale flat price discount with fractional bulk quantity', target: new LimitedUpSaleFlatPriceDiscount(new Date(), new Date(), 'by quantity item', 0.5, 1, 1, 10) },
            { description: 'up sale percent discount by weight with fractional bulk quantity', target: new UpSalePercentDiscountByWeight(new Date(), new Date(), 'by weight item', 0.25, 1, .5) }
        ]);

        invalidFractionalBulkQuantityDiscountScenarios.forEach().describe('Given a', (invalidDiscountScenario: TestScenario<Discount>) => {

            const scenarioDescription: string = invalidDiscountScenario.description;
            const invalidDiscount: Discount = invalidDiscountScenario.target;
            const fakeItemList: FakeItemList = new FakeItemList();
            const itemList: ItemList = fakeItemList;
            const expectedError: Error = new Error('Bulk Quantity must be a Whole Number');

            describe(scenarioDescription, () => {

                fakeItemList.addFakeItem('by quantity item', new FakeByQuantityItem());
                fakeItemList.addFakeItem('by weight item', new FakeByWeightItem());

                describe('When validating', () => {
                    let error: Error | null = null;

                    beforeEach(() => {
                        try {
                            invalidDiscount.validate(itemList);
                        } catch (exception) {
                            error = exception;
                        }
                    });

                    it('Should raise an error.', () => {
                        expect(error).not.toBeNull();
                        expect(error).toEqual(expectedError);
                    });

                });

            });

        });

        const invalidFractionalSaleQuantityDiscountScenarios: Parameterized<Discount, TestScenario<Discount>> = new Parameterized<Discount, TestScenario<Discount>>([
            { description: 'up sale percent discount with fractional sale quantity', target: new UpSalePercentDiscount(new Date(), new Date(), 'by quantity item', 2, 1.25, 0.5) },
            { description: 'limited up sale percent discount with fractional sale quantity', target: new LimitedUpSalePercentDiscount(new Date(), new Date(), 'by quantity item', 25, 5.5, 1, 8) },
            { description: 'up sale flat price discount with fractional sale quantity', target: new UpSaleFlatPriceDiscount(new Date(), new Date(), 'by quantity item', 1, 0.25, 1.25) },
            { description: 'limited up sale flat price discount with fractional sale quantity', target: new LimitedUpSaleFlatPriceDiscount(new Date(), new Date(), 'by quantity item', 5, 0.9999, 1, 10) },
            { description: 'up sale percent discount by weight with fractional sale quantity', target: new UpSalePercentDiscountByWeight(new Date(), new Date(), 'by weight item', 5, 1.2342, .5) }
        ]);

        invalidFractionalSaleQuantityDiscountScenarios.forEach().describe('Given a', (invalidDiscountScenario: TestScenario<Discount>) => {

            const scenarioDescription: string = invalidDiscountScenario.description;
            const invalidDiscount: Discount = invalidDiscountScenario.target;
            const fakeItemList: FakeItemList = new FakeItemList();
            const itemList: ItemList = fakeItemList;
            const expectedError: Error = new Error('Sale Quantity must be a Whole Number');

            describe(scenarioDescription, () => {

                fakeItemList.addFakeItem('by quantity item', new FakeByQuantityItem());
                fakeItemList.addFakeItem('by weight item', new FakeByWeightItem());

                describe('When validating', () => {
                    let error: Error | null = null;

                    beforeEach(() => {
                        try {
                            invalidDiscount.validate(itemList);
                        } catch (exception) {
                            error = exception;
                        }
                    });

                    it('Should raise an error.', () => {
                        expect(error).not.toBeNull();
                        expect(error).toEqual(expectedError);
                    });

                });

            });

        });

    }

}
