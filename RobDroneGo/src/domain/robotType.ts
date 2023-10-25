import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";
import {RobotTypeId} from "./robotTypeId";
import IRobotTypeDTO from "../dto/IRobotTypeDTO";


interface RobotTypeProps {
    designation: string;
}

export class RobotType extends AggregateRoot<RobotTypeProps> {
    get id(): UniqueEntityID {
        return this._id;
    }

    get robotTypeId(): RobotTypeId {
        return new RobotTypeId(this.robotTypeId.toValue());
    }

    get designation(): string {
        return this.props.designation;
    }

    private constructor(props: RobotTypeProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create(robotTypeDTO: IRobotTypeDTO, id?: UniqueEntityID): Result<RobotType> {
        const designation = robotTypeDTO.designation;


        const robot = new RobotType({
            designation: designation
        }, id);
        return Result.ok<RobotType>(robot);
    }

}

