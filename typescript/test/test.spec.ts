/* global describe, it, expect, beforeEach */

import { ItemList, Item } from '../src/item';

interface Parameterized<T> {
    it(description: string, func: (value: T) => void): void;
    describe(description: string, func: (value: T) => void): void;
}

class Parameters<T> {
    constructor(public readonly cases: Array<T>) {}
    public forEach(): Parameterized<T> { 
        return {
            it: (description: string, func: (value: T) => void): void => {
                this.cases.forEach((cs: T): void => {
                    it(description, (): void => {
                        func(cs);
                    });     
                });
            },
            describe: (description: string, func: (value: T) => void): void => {
                this.cases.forEach((cs: T): void => {
                    describe(description, (): void => {
                        func(cs);
                    });     
                });
            }
        };
    }
}

describe('Given a collection of Items', () => {
    const itemList = new ItemList();

    describe('And a new item containing an Item Code, Description, Type, and Price', () => {
        const item = new Item(
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

    });

    const invalidItems: Parameters<[string, Item]> = new Parameters<[string, Item]>([
        ['item code', new Item(null, 'random description', 'by weight', 3.0)],
        ['description', new Item('random item code', null, 'by quantity', 3.0)],
        ['type', new Item('random item code', 'random description', null, 3.0)],
        ['price', new Item('random item code', 'random description', 'by weight', null)]
    ]);

    invalidItems.forEach().describe('And an invalid item When adding it, it', (item: [string, Item]) => {
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