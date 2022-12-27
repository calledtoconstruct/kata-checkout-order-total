
import { Parameterized, TestScenario } from './parameterized';
import { ItemList, Item, ItemListImplementation, StandardItem } from '../src/item';

export class TestItemList {

    public static scenarios(): void {

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
        
                    beforeEach(async (): Promise<void> => {
                        await itemList.add(item);
                    });
        
                    it('Should exist in the list', async (): Promise<void> => {
                        const result = await itemList.includes(item);
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
        
                    beforeEach(async (): Promise<void> => {
                        await itemList.add(item);
                        await itemList.add(otherItem);
                    });
        
                    it('the first item should not exist in the list', async (): Promise<void> => {
                        const result = await itemList.includes(item);
                        expect(result).toBe(false);
                    });
        
                    it('the other item should exist in the list', async (): Promise<void> => {
                        const result = await itemList.includes(otherItem);
                        expect(result).toBe(true);
                    });

                });

            });

            const invalidItems: Parameterized<Item, TestScenario<Item>> = new Parameterized<Item, TestScenario<Item>>([
                { description: 'item code', target: new StandardItem('', 'random description', 'by weight', 3.0) },
                { description: 'description', target: new StandardItem('random item code', '', 'by quantity', 3.0) },
                { description: 'price', target: new StandardItem('random item code', 'random description', 'by weight', 0) }
            ]);

            invalidItems.forEach().describe('And an invalid item When adding it, it', (invalidItemScenario: TestScenario<Item>) => {

                const scenarioDescription: string = invalidItemScenario.description;
                const invalidItem: Item = invalidItemScenario.target;
                let error: Error | null = null;
        
                beforeEach(async (): Promise<void> => {
                    try {
                        await itemList.add(invalidItem);
                        fail();
                    } catch (caught) {
                        error = (caught instanceof Error)
                            ? caught
                            : new Error("Unexpected exception");
                    }
                });

                it('Should reject the item because it is missing ' + scenarioDescription, () => {
                    expect(error).not.toBeNull()
                });
        
                it('Should not be added to the list because it is missing ' + scenarioDescription, async (): Promise<void> => {
                    const result = await itemList.includes(invalidItem);
                    expect(result).toBe(false);
                });

            });

        });

    }

}
