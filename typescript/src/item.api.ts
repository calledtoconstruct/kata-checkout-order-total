
import * as express from 'express';
import * as cors from 'cors';
import * as env from './env';
import { ItemListImplementation, ItemList, Item, StandardItem, Priced } from './item';
import { asyncForEach } from './async.for.each';

const application = express();
const port: number = env.ITEM_API_PORT;

application.use(cors());

const itemList: ItemList = new ItemListImplementation();

const items: Array<Item> = [
    new StandardItem('dog food', 'A 32oz package of premium dog food.', 'by quantity', 5.00),
    new StandardItem('cat food', 'A 12oz can of premium cat food.', 'by quantity', 1.25),
    new StandardItem('ground beef', 'Ground Chuck', 'by weight', 2.00)
];

const addItemPromise: Promise<void> = asyncForEach(items, async (item: Item): Promise<void> => {
    try {
        await itemList.add(item);
    } catch (error) {
        if (error instanceof Error) {
            console.log(error.message);
        }
    }
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
