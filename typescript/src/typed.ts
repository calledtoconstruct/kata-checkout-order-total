
export class Typed<Thing> {
    constructor(public readonly type: string, public readonly thing: Thing) {
    }
}

export interface TypeFactory<Interface> {
    type(instance: Interface): Typed<Interface>;
    make(from: Typed<Interface>): Interface;
}
