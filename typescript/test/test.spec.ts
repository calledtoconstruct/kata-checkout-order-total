/* global describe, it, expect, beforeEach */

import { ItemList, Item } from '../src/item';

class Parameters<T> {
    constructor(public readonly cases: Array<T>) {}
    public forEach(): any { 
        return {
            it: (description: string, func: (value: T) => void): void => {
                this.cases.forEach((cs: T): void => {
                    it(description + ' ' + JSON.stringify(cs), (): void => {
                        func(cs);
                    });     
                });
            },
            describe: (description: string, func: (value: T) => void): void => {
                this.cases.forEach((cs: T): void => {
                    describe(description + ' ' + JSON.stringify(cs), (): void => {
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
            'random description'
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

    describe('And an invalid item missing an Item Code, Description, Type, or Price', () => {

        describe('When adding an item', () => {
            let error: Error | null = null;

            const parameters: Parameters<[string, Item]> = new Parameters<[string, Item]>([
                ['item code', new Item(null, 'random description')],
                ['description', new Item('random item code', null)]
            ]);

            const add = (item: Item) => {
                try {
                    itemList.add(item);
                    fail();
                } catch (caught) {
                    error = caught;
                }
            };

            parameters.forEach().describe('missing', (item: Item) => {

                beforeEach(() => {
                    add(item);
                });
    
                it('Should reject the item because it is missing', () => {
                    expect(error).not.toBeNull()
                });

                it('Should not be added to list', () => {
                    const result = itemList.includes(item);
                    expect(result).toBe(false);
                });

            });
        });
    });
});