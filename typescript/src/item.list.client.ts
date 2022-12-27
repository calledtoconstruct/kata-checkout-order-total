
import { ItemList, Item, Priced } from './item';
import { sendRequest } from './api.client';
import * as env from './env';

export class ItemListClient implements ItemList {

    async get(code: string): Promise<Item & Priced | undefined> {
        try {
            const result = await sendRequest(`http://${env.ITEM_API_BASE_URL}:${env.ITEM_API_PORT}/item/${code}`);
            return <Item & Priced>result;
        } catch (error) {
            if (error === 404) {
                return undefined;
            }
            throw error;
        }
    }

    async add(_: Item): Promise<void> {
        throw new Error('Method not implemented.');
    }

    async includes(_: Item): Promise<boolean> {
        throw new Error('Method not implemented.');
    }

}
