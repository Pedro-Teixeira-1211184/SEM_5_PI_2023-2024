import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";
import {PassagewayId} from "./passagewayId";

import IPassagewayDTO from "../dto/IPassagewayDTO";

interface PassagewayProps {
    floorCode1: string;
    floorCode2: string;
    localization1: string;
    localization2: string;
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

    get localization1(): string {
        return this.props.localization1;
    }

    get localization2(): string {
        return this.props.localization2;
    }

    set localization1(value: string) {
        this.props.localization1 = value;
    }

    set localization2(value: string) {
        this.props.localization2 = value;
    }


    private constructor(props: PassagewayProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create(passagewayDTO: IPassagewayDTO, id?: UniqueEntityID): Result<Passageway> {
        const floorCode1 = passagewayDTO.floorCode1;
        const floorCode2 = passagewayDTO.floorCode2;
        const localization1 = passagewayDTO.localization1;
        const localization2 = passagewayDTO.localization2;

        const passageway = new Passageway({
            floorCode1: floorCode1,
            floorCode2: floorCode2,
            localization1: localization1,
            localization2: localization2
        }, id);
        return Result.ok<Passageway>(passageway);
    }

}
