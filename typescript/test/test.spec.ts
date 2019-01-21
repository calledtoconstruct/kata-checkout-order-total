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
                const result = itemList.contains(item);
                expect(result).toBe(true);
            });

        });

    });

    describe('And an invalid item missing an Item Code, Description, Type, or Price', () => {
        const item = new Item(null
        );

        describe('When adding the item', () => {

            beforeEach(() => {
            });

            it('Should reject the item because it is missing the Item Code', () => {
                try {
                    itemList.add(item);
                    fail();
                } catch (error) {
                    
                }
            });
        });
    });
});