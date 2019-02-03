
import * as express from 'express';
import * as cors from 'cors';
import * as env from './env';
import { DiscountListImplementation, DiscountList, Discount, StandardDiscount, UpSalePercentDiscountByWeight, DiscountTypeFactory } from './discount';
import { ItemList } from './item';
import { ItemListClient } from './item.list.client';
import { TypeFactory, Typed } from './typed';

const application = express();
const port: number = env.DISCOUNT_API_PORT;

application.use(cors());

const itemList: ItemList = new ItemListClient();
const discountList: DiscountList = new DiscountListImplementation(itemList);
const typeFactory: TypeFactory<Discount> = new DiscountTypeFactory();

const startDate: Date = new Date(new Date().valueOf() - 10);
const endDate: Date = new Date(new Date().valueOf() + 10);

const discounts: Array<Discount> = [
    new StandardDiscount(startDate, endDate, 'cat food', 1.00),
    new UpSalePercentDiscountByWeight(startDate, endDate, 'ground beef', 2, 1, .5)
];

discounts.forEach((discount: Discount): void => {
    try {
        discountList.add(discount);
    } catch (error) {
        if (error instanceof Error) {
            console.log(error.message);
        }
    }
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
            const typed: Typed<Discount> = typeFactory.type(discount);
            response.jsonp(typed);
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
