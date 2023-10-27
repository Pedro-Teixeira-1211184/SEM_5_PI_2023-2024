import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";

import { Result } from "../core/logic/Result";
import { PassagewayId } from "./passagewayId";

import IPassagewayDTO from "../dto/IPassagewayDTO";

interface PassagewayProps {
    floorID1: string;
    floorID2: string;
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

    get floorID1(): string {
        return this.props.floorID1;
    }

    get floorID2(): string {
        return this.props.floorID2;
    }

    get localization1(): string {
        return this.props.localization1;
    }

    get localization2(): string {
        return this.props.localization2;
    }

    private constructor(props: PassagewayProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create(passagewayDTO: IPassagewayDTO, id?: UniqueEntityID): Result<Passageway> {
        const floorID1 = passagewayDTO.floorID1;
        const floorID2 = passagewayDTO.floorID2;
        const localization1 = passagewayDTO.localization1;
        const localization2 = passagewayDTO.localization2;

        const passageway = new Passageway({
          floorID1: floorID1,
          floorID2: floorID2,
          localization1: localization1,
          localization2: localization2
        }, id);
        return Result.ok<Passageway>(passageway);
      }

}
