/* global describe, it, expect, beforeEach */

import { ItemList, Item } from '../src/item';

describe('Given a collection of Items', () => {
    const itemList = new ItemList();

    describe('And a new item containing an Item Code, Description, Type, and Price', () => {
        const item = new Item(
            'random item code'
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

            const add = (item: Item) => {
                try {
                    itemList.add(item);
                    fail();
                } catch (caught) {
                    error = caught;
                }
            };

            describe('missing Item Code', () => {
                const item = new Item(null);

                beforeEach(() => {
                    add(item);
                });
    
                it('Should reject the item because it is missing the Item Code', () => {
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