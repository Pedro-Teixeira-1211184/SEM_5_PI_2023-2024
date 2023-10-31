import {Mapper} from "../core/infra/Mapper";
import {Document, Model} from 'mongoose';

import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {Floor} from "../domain/floor";
import {IFloorPersistence} from "../dataschema/IFloorPersistence";
import IFloorDTO from "../dto/IFloorDTO";

export class FloorMapper extends Mapper<Floor> {

    public static toDTO(floor: Floor): IFloorDTO {
        return {
            id: floor.id.toString(),
            buildingCode: floor.buildingCode,
            number: floor.number,
            code: floor.code,
            description: floor.description
        } as IFloorDTO;
    }


    public static toDomain(raw: any | Model<IFloorPersistence & Document>): Floor {
        const floorOrError = Floor.create({
            id: raw.floorID,
            buildingCode: raw.floorBuildingCode,
            number: raw.floorNumber,
            code: raw.floorCode,
            description: raw.floorDescription
        }, new UniqueEntityID(raw.floorID));

        floorOrError.isFailure ? console.log(floorOrError.error) : '';

        return floorOrError.isSuccess ? floorOrError.getValue() : null;
    }

    public static toPersistence(floor: Floor): any {
        return {
            floorID: floor.id.toString(),
            floorBuildingCode: floor.buildingCode,
            floorNumber: floor.number,
            floorCode: floor.code,
            floorDescription: floor.description
        }
    }
}
