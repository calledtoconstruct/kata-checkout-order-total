/* global describe, it, expect, beforeEach */

import { Discount } from '../src/discount';
import { Item } from '../src/item';
import { Transaction } from '../src/transaction';

describe('...', () => {
    describe('...', () => {
        const discount = new Discount();
        const item = new Item();
        const transaction = new Transaction();

        beforeEach(() => {

        });

        it('...', () => {
            const result = discount.test();
            expect(result).toBe(false);
        });

        it('...', () => {
            const result = item.test();
            expect(result).toBe(false);
        });

        it('...', () => {
            const result = transaction.test();
            expect(result).toBe(false);
        });
    });
});