import {Mapper} from "../core/infra/Mapper";
import {Document, Model} from 'mongoose';

import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {RobotType} from "../domain/robotType";
import IRobotTypeDTO from "../dto/IRobotTypeDTO";
import {IRobotTypePersistence} from "../dataschema/IRobotTypePersistence";

export class RobotTypeMapper extends Mapper<RobotType> {

    public static toDTO(robotType: RobotType): IRobotTypeDTO {
        return {
            id: robotType.id.toString(),
            designation: robotType.designation.toString()
        } as IRobotTypeDTO;
    }


    public static toDomain(raw: any | Model<IRobotTypePersistence & Document>): RobotType {
        const typeOrError = RobotType.create({
            id: raw.robotTypeID,
            designation: raw.robotTypeDesignation
        }, new UniqueEntityID(raw.robotTypeID));

        typeOrError.isFailure ? console.log(typeOrError.error) : '';

        return typeOrError.isSuccess ? typeOrError.getValue() : null;
    }

    public static toPersistence(robotType: RobotType): any {
        return {
            robotTypeID: robotType.id.toString(),
            robotTypeDesignation: robotType.designation.toString()
        }
    }
}
