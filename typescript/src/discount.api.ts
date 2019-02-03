
import * as express from 'express';
import * as cors from 'cors';
import { DiscountListImplementation, DiscountList, Discount } from './discount';
import { ItemList } from './item';
import { ItemListClient } from './item.list.client';

const application = express();
const port: number = 8082;

application.use(cors());

const itemList: ItemList = new ItemListClient();
const discountList: DiscountList = new DiscountListImplementation(itemList);

application.get('/', async (_: express.Request, response: express.Response): Promise<void> => {
    response.sendStatus(200);
});

application.get('/discount/:date/:code', async (request: express.Request, response: express.Response): Promise<void> => {
    const date: Date = new Date(request.params.date);
    const code: string = request.params.code;
    try {
        const discount: Discount | undefined = await discountList.get(date, code);
        if (discount !== undefined) {
            response.jsonp(discount);
        } else {
            response.sendStatus(404);
        }
    } catch (exception) {
        response.sendStatus(500);
    }
});

application.listen(port, (): void => {
    console.log('Discount API Listening.');
});
