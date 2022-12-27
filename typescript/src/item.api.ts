
import * as express from 'express';
import * as cors from 'cors';
import * as env from './env';
import { ItemListImplementation, ItemList, Item, StandardItem, Priced } from './item';
import { asyncForEach } from './async.for.each';
import { readFile } from 'jsonfile';

const application = express();
const port: number = env.ITEM_API_PORT;

application.use(cors());

const itemList: ItemList = new ItemListImplementation();

const addItemPromise: Promise<void> = readFile('./items.json')
    .then((items: Array<Item>): Promise<void> => {
        return asyncForEach(items, async (item: Item): Promise<void> => {
            try {
                const castedItem = <StandardItem>item;
                const standardItem = new StandardItem(
                    castedItem.itemCode,
                    castedItem.itemDescription,
                    castedItem.itemType,
                    castedItem.itemPrice
                );
                await itemList.add(standardItem);
            } catch (error) {
                if (error instanceof Error) {
                    console.log(error.message);
                }
            }
        });
    });

application.get('/', async (_: express.Request, response: express.Response): Promise<void> => {
    response.sendStatus(200);
    return Promise.resolve();
});

application.get('/item/:code', async (request: express.Request, response: express.Response): Promise<void> => {
    const code: string = request.params.code;
    try {
        const item: (Item & Priced) | undefined = await itemList.get(code);
        response.jsonp(item);
    } catch (exception) {
        response.sendStatus(404);
    }
});

addItemPromise.then(() => {
    application.listen(port, (): void => {
        console.log('Item API Listening.');
    });
});
