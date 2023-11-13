import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";
import {PassagewayId} from "./passagewayId";

import IPassagewayDTO from "../dto/IPassagewayDTO";

interface PassagewayProps {
    floorCode1: string;
    floorCode2: string;
}

export class Passageway extends AggregateRoot<PassagewayProps> {
    get id(): UniqueEntityID {
        return this._id;
    }

    get passagewayId(): PassagewayId {
        return new PassagewayId(this.passagewayId.toValue());
    }

    get floorCode1(): string {
        return this.props.floorCode1;
    }

    get floorCode2(): string {
        return this.props.floorCode2;
    }

    set floorCode1(floorCode: string) {
        this.props.floorCode1 = floorCode;
    }

    set floorCode2(floorCode: string) {
        this.props.floorCode2 = floorCode;
    }

    private constructor(props: PassagewayProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create(passagewayDTO: IPassagewayDTO, id?: UniqueEntityID): Result<Passageway> {
        const floorCode1 = passagewayDTO.floorCode1;
        const floorCode2 = passagewayDTO.floorCode2;
        const passageway = new Passageway({
            floorCode1: floorCode1,
            floorCode2: floorCode2,
        }, id);
        return Result.ok<Passageway>(passageway);
    }

}
