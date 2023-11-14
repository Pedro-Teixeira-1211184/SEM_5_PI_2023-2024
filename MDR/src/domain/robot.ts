import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";

import {Result} from "../core/logic/Result";
import {RobotId} from "./robotId";

import IRobotDTO from "../dto/IRobotDTO";
import {FloorId} from "./floorId";

interface RobotProps {
    robotType: string;
    code: string;
    serialNumber: string;
    nickname: string;
    brand: string;
    isActive: boolean;
}

export class Robot extends AggregateRoot<RobotProps> {
    get id(): UniqueEntityID {
        return this._id;
    }

    get robotId(): RobotId {
        return new RobotId(this.robotId.toValue());
    }

    get robotType(): string {
        return this.props.robotType;
    }

    get code(): string {
        return this.props.code;
    }

    get serialNumber(): string {
        return this.props.serialNumber;
    }

    get nickname(): string {
        return this.props.nickname;
    }

    get brand(): string {
        return this.props.brand;
    }

    get isActive(): boolean {
        return this.props.isActive;
    }

    set isActive(value: boolean) {
        this.props.isActive = value;
    }

    private constructor(props: RobotProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create(robotDTO: IRobotDTO, id?: UniqueEntityID): Result<Robot> {
        const robotType = robotDTO.robotType;
        const code = robotDTO.code;
        const serialNumber = robotDTO.serialNumber;
        const nickname = robotDTO.nickname;
        const brand = robotDTO.brand;
        const isActive = robotDTO.isActive;


        const robot = new Robot({
            robotType: robotType,
            code: code,
            serialNumber: serialNumber,
            nickname: nickname,
            brand: brand,
            isActive: isActive
        }, id);
        return Result.ok<Robot>(robot);
    }

}

