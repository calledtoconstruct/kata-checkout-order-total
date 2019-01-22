
import { Parameterized, TestScenario } from './parameterized';

import { ItemList, Item, ItemListImplementation, StandardItem, ItemType } from '../src/item';

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

type DateRange = { startDate: Date, endDate: Date };

describe('Given a collection of Items', () => {
    let itemList: ItemList;

    beforeEach(() => {
        itemList = new ItemListImplementation();
    });

    describe('And a new item containing an Item Code, Description, Type, and Price', () => {
        const item = new StandardItem(
            'random item code',
            'random description',
            'by quantity',
            3.0
        );

        describe('When adding the item', () => {

            beforeEach(() => {
                itemList.add(item);
            });

            it('Should exist in the list', () => {
                const result = itemList.includes(item);
                expect(result).toBe(true);
            });

        });

        describe('When adding the item again', () => {
            const otherItem = new StandardItem(
                'random item code',
                'random description',
                'by quantity',
                5.0
            );

            beforeEach(() => {
                itemList.add(item);
                itemList.add(otherItem);
            });

            it('the first item should not exist in the list', () => {
                const result = itemList.includes(item);
                expect(result).toBe(false);
            });

            it('the other item should exist in the list', () => {
                const result = itemList.includes(otherItem);
                expect(result).toBe(true);
            });

        });

    });

    const invalidItems: Parameterized<Item, TestScenario<Item>> = new Parameterized<Item, TestScenario<Item>>([
        ['item code', new StandardItem(null, 'random description', 'by weight', 3.0)],
        ['description', new StandardItem('random item code', null, 'by quantity', 3.0)],
        ['type', new StandardItem('random item code', 'random description', null, 3.0)],
        ['price', new StandardItem('random item code', 'random description', 'by weight', null)]
    ]);

    invalidItems.forEach().describe('And an invalid item When adding it, it', (item: TestScenario<Item>) => {
        let error: Error | null = null;

        beforeEach(() => {
            try {
                itemList.add(item[1]);
                fail();
            } catch (caught) {
                error = caught;
            }
        });

        it('Should reject the item because it is missing ' + item[0], () => {
            expect(error).not.toBeNull()
        });

        it('Should not be added to the list', () => {
            const result = itemList.includes(item[1]);
            expect(result).toBe(false);
        });

    });
    
});

describe('Given a collection of Pricing Rules', () => {
    class FakeByQuantityItem implements Item {
        public readonly code: string = 'by quantity item';
        public readonly type: ItemType = 'by quantity';
        public validate(): void {}
    }

    class FakeByWeightItem implements Item {
        public readonly code: string = 'by weight item';
        public readonly type: ItemType = 'by weight';
        public validate(): void {}
    }

    class FakeItemList implements ItemList {
        public add(item: Item): void {
            throw new Error("Method not implemented.");
        }
        public includes(item: Item): boolean {
            throw new Error("Method not implemented.");
        }
        public get(code: string): Item {
            if (code === 'by quantity item') {
                return new FakeByQuantityItem();
            } else if (code === 'by weight item') {
                return new FakeByWeightItem();
            } else throw new Error('invalid test');
        }
    }

    let discountList: DiscountList;

    beforeEach(() => {
        const itemList: ItemList = new FakeItemList()
        discountList = new DiscountListImplementation(itemList);
    });

    const discountMonth: DateRange = {
        startDate: new Date(2010, 3, 1, 0, 0, 0, 0),
        endDate: new Date(2010, 3, 31, 11, 59, 59, 999)
    };

    const validDiscountScenarios: Parameterized<Discount, TestScenario<Discount>> = new Parameterized<Discount, TestScenario<Discount>>([
        ['standard discount', new StandardDiscount(discountMonth.startDate, discountMonth.endDate, 'by quantity item', 1.0)],
        ['bulk flat price', new BulkFlatPriceDiscount(discountMonth.startDate, discountMonth.endDate, 'by quantity item', 3, 5.0)],
        ['up sale percent discount', new UpSalePercentDiscount(discountMonth.startDate, discountMonth.endDate, 'by quantity item', 2, 1, 0.5)],
        ['limited up sale percent discount', new LimitedUpSalePercentDiscount(discountMonth.startDate, discountMonth.endDate, 'by quantity item', 3, 1, 1, 8)],
        ['up sale flat price discount', new UpSaleFlatPriceDiscount(discountMonth.startDate, discountMonth.endDate, 'by quantity item', 2, 1, 1.25)],
        ['limited up sale flat price discount', new LimitedUpSaleFlatPriceDiscount(discountMonth.startDate, discountMonth.endDate, 'by quantity item', 3, 2, 1, 10)],
        ['up sale percent discount by weight', new UpSalePercentDiscountByWeight(discountMonth.startDate, discountMonth.endDate, 'by weight item', 2, 1, .5)]
    ]);

    validDiscountScenarios.forEach().describe('And a valid discount When adding it', (discountScenario: TestScenario<Discount>) => {

        beforeEach(() => {
            discountList.add(discountScenario[1]);
        });

        it('Should be added to the list', () => {
            const result = discountList.includes(discountScenario[1]);
            expect(result).toBe(true);
        });

    });

    const invalidDiscountsWithItemTypeMismatch: Parameterized<Discount, TestScenario<Discount>> = new Parameterized<Discount, TestScenario<Discount>>([
        ['standard discount item type mismatch', new StandardDiscount(new Date(), new Date(), 'by weight item', 1.0)],
        ['bulk flat price item type mismatch', new BulkFlatPriceDiscount(new Date(), new Date(), 'by weight item', 3, 5.0)],
        ['up sale percent discount item type mismatch', new UpSalePercentDiscount(new Date(), new Date(), 'by weight item', 2, 1, 0.5)],
        ['limited up sale percent discount item type mismatch', new LimitedUpSalePercentDiscount(new Date(), new Date(), 'by weight item', 3, 1, 1, 8)],
        ['up sale flat price discount item type mismatch', new UpSaleFlatPriceDiscount(new Date(), new Date(), 'by weight item', 2, 1, 1.25)],
        ['limited up sale flat price discount item type mismatch', new LimitedUpSaleFlatPriceDiscount(new Date(), new Date(), 'by weight item', 3, 2, 1, 10)],
        ['up sale percent discount by weight item type mismatch', new UpSalePercentDiscountByWeight(new Date(), new Date(), 'by quantity item', 2, 1, .5)],
        ['standard discount no matching item', new StandardDiscount(new Date(), new Date(), 'no matching item', 1.0)],
        ['bulk flat price no matching item', new BulkFlatPriceDiscount(new Date(), new Date(), 'no matching item', 3, 5.0)],
        ['up sale percent discount no matching item', new UpSalePercentDiscount(new Date(), new Date(), 'no matching item', 2, 1, 0.5)],
        ['limited up sale percent discount no matching item', new LimitedUpSalePercentDiscount(new Date(), new Date(), 'no matching item', 3, 1, 1, 8)],
        ['up sale flat price discount no matching item', new UpSaleFlatPriceDiscount(new Date(), new Date(), 'no matching item', 2, 1, 1.25)],
        ['limited up sale flat price discount no matching item', new LimitedUpSaleFlatPriceDiscount(new Date(), new Date(), 'no matching item', 3, 2, 1, 10)],
        ['up sale percent discount by weight no matching item', new UpSalePercentDiscountByWeight(new Date(), new Date(), 'no matching item', 2, 1, .5)]
    ]);

    invalidDiscountsWithItemTypeMismatch.forEach().describe('And an invalid discount When adding it', (typeMismatchScenario: TestScenario<Discount>) => {
        let error: Error | null = null;

        beforeEach(() => {
            try {
                discountList.add(typeMismatchScenario[1]);
            } catch (exception) {
                error = exception;
            }
        });

        it(typeMismatchScenario[0] + ' Should raise an error', () => {
            expect(error).not.toBeNull();
        });

        it(typeMismatchScenario[0] + ' Should not be added to the list', () => {
            const result = discountList.includes(typeMismatchScenario[1]);
            expect(result).toBe(false);
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
        ['week inside of month', weekInsideMonth],
        ['week overlapping beginning of month', weekOverlappingBeginningOfMonth],
        ['week overlapping end of month', weekOverlappingEndOfMonth],
        ['quarter overlapping entire month', quarterOverlappingEntireMonth]
    ]);

    overlappingDateScenarios.forEach().describe('When adding a duplicate', (overlappingDateScenario: TestScenario<DateRange>) => {  
        
        const overlappingDateRange: DateRange = overlappingDateScenario[1];

        const overlappingDiscountScenarios: Parameterized<Discount, TestScenario<Discount>> = new Parameterized<Discount, TestScenario<Discount>>([
            ['standard discount', new StandardDiscount(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by quantity item', 1.0)],
            ['bulk flat price', new BulkFlatPriceDiscount(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by quantity item', 3, 5.0)],
            ['up sale percent discount', new UpSalePercentDiscount(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by quantity item', 2, 1, 0.5)],
            ['limited up sale percent discount', new LimitedUpSalePercentDiscount(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by quantity item', 3, 1, 1, 8)],
            ['up sale flat price discount', new UpSaleFlatPriceDiscount(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by quantity item', 2, 1, 1.25)],
            ['limited up sale flat price discount', new LimitedUpSaleFlatPriceDiscount(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by quantity item', 3, 2, 1, 10)],
            ['up sale percent discount by weight', new UpSalePercentDiscountByWeight(overlappingDateRange.startDate, overlappingDateRange.endDate, 'by weight item', 2, 1, .5)]
        ]);

        overlappingDiscountScenarios.forEach().describe(overlappingDateScenario[0], (overlappingDiscountScenario: TestScenario<Discount>) => {

            const overlappingDiscount: Discount = overlappingDiscountScenario[1];
            const whenSameCode = (validDiscount: TestScenario<Discount>): boolean => validDiscount[1].code === overlappingDiscount.code;

            validDiscountScenarios.forEach(whenSameCode).describe(overlappingDiscountScenario[0], (discountScenario: TestScenario<Discount>) => {

                const discount: Discount = discountScenario[1];
                let error: Error | null = null;

                beforeEach(() => {
                    discountList.add(discount);
                    try {
                        discountList.add(overlappingDiscount);
                    } catch (exception) {
                        error = exception;
                    }
                });

                describe(discountScenario[0], () => {

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