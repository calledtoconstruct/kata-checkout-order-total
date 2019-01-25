
export class Currency {

    public static floor(value: number): number {
        return Math.floor(value * 100) / 100;
    }
    
}