import {Mapper} from "../core/infra/Mapper";
import {Document, Model} from 'mongoose';
import {Robot} from "../domain/robot";

import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {IRobotPersistence} from "../dataschema/IRobotPersistence";
import IRobotDTO from "../dto/IRobotDTO";

export class RobotMapper extends Mapper<Robot> {

    public static toDTO(robot: Robot): IRobotDTO {
        return {
            id: robot.id.toString(),
            robotType: robot.robotType,
            code: robot.code,
            serialNumber: robot.serialNumber,
            nickname: robot.nickname,
            brand: robot.brand,
            isActive: robot.isActive
        } as IRobotDTO;
    }


    public static toDomain(raw: any | Model<IRobotPersistence & Document>): Robot {
        const robot = Robot.create(
            {
                id: raw.robotID,
                robotType: raw.robotType,
                code: raw.robotCode,
                serialNumber: raw.robotSerialNumber,
                nickname: raw.robotNickname,
                brand: raw.robotBrand,
                isActive: raw.robotIsActive
            },
            new UniqueEntityID(raw.domainId)
        );


        robot.isFailure ? console.log(robot.error) : '';

        return robot.isSuccess ? robot.getValue() : null;
    }

    public static toPersistence(robot: Robot): any {
        return {
            robotID: robot.id.toString(),
            robotRobotType: robot.robotType.toString(),
            robotCode: robot.code.toString(),
            robotSerialNumber: robot.serialNumber.toString(),
            robotNickname: robot.nickname.toString(),
            robotBrand: robot.brand.toString(),
            robotIsActive: robot.isActive.valueOf()
        }
    }
}
