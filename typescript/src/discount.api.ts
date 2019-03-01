
import * as express from 'express';
import * as cors from 'cors';
import * as env from './env';
import { DiscountListImplementation, DiscountList, Discount, DiscountTypeFactory } from './discount';
import { ItemList } from './item';
import { ItemListClient } from './item.list.client';
import { readFile } from 'jsonfile';
import { asyncForEach } from './async.for.each';

const application = express();
const port: number = env.DISCOUNT_API_PORT;

application.use(cors());

const itemList: ItemList = new ItemListClient();
const discountList: DiscountList = new DiscountListImplementation(itemList);
const discountTypeFactory: DiscountTypeFactory = new DiscountTypeFactory();

const discountsLoaded: Promise<void> = readFile('discounts.json')
    .then((discounts: Array<Discount>): Promise<void> => {
        return asyncForEach(discounts, async (discount: Discount): Promise<void> => {
            const mappedDiscount: Discount = discountTypeFactory.get(discount);
            return await discountList.add(mappedDiscount);
        });
    });

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

discountsLoaded.then(() => {
    application.listen(port, (): void => {
        console.log('Discount API Listening.');
    });
});
