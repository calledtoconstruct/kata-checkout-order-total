
var xhrRequest = require('xhr-request');

const createClient = (): any => {
    let client: any = xhrRequest;
    return client;
}

export const sendRequest = async <Response>(url: string, postData?: any): Promise<Response> => {
    return new Promise<Response>((resolve: (response: Response) => void, reject: (reason?: any) => void): void => {

        var client: any = createClient();

        if (client) {
            const method: string = (postData) ? "POST" : "GET";

            const headers: any = (postData) ? {
                'Content-type': 'application/x-www-form-urlencoded'
            } : {}

            client(url, {
                headers: headers,
                method: method,
                json: true,
                body: postData,
                responseType: 'text'
            }, (err: number, data: any): void => {
                if (err) {
                    if (err === 404) {
                        reject(err);
                    }
                } else {
                    if (data === 'Not Found') {
                        reject(404);
                    } else {
                        resolve(JSON.parse(data));
                    }

                }

            });

        }

    });

}
