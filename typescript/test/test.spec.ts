/* global describe, it, expect, beforeEach */

import { ItemList, Item } from '../src/item';

describe('Given a collection of Items', () => {
    const itemList = new ItemList();

    describe('And a new item containing an Item Code, Description, Type, and Price', () => {
        const item = new Item();

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
});