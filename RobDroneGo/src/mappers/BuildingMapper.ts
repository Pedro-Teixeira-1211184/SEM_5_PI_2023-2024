import {Mapper} from "../core/infra/Mapper";
import {Document, Model} from 'mongoose';
import {IBuildingPersistence} from '../dataschema/IBuildingPersistence';
import IBuildingDTO from "../dto/IBuildingDTO";
import {Building} from "../domain/building";

import {UniqueEntityID} from "../core/domain/UniqueEntityID";

export class BuildingMapper extends Mapper<Building> {

    public static toDTO(building: Building): IBuildingDTO {
        return {
            buildingID: building.id.toString(),
            buildingDimensions: building.dimensions.toString(),
            buildingCode: building.code.toString(),
            buildingName: building.name.toString(),
            buildingDescription: building.description.toString()
        } as unknown as IBuildingDTO;
    }


    public static toDomain(raw: any | Model<IBuildingPersistence & Document>): Building {
        const buildingOrError = Building.create(
            {
                buildingID: raw.buildingID,
                buildingCode: raw.buildingCode,
                buildingDimensions: raw.buildingDimensions,
                buildingName: raw.buildingName,
                buildingDescription: raw.buildingDescription
            },
            new UniqueEntityID(raw.domainId)
        );

        buildingOrError.isFailure ? console.log(buildingOrError.error) : '';

        return buildingOrError.isSuccess ? buildingOrError.getValue() : null;
    }

    public static toPersistence(building: Building): any {
        return {
            buildingID: building.id.toString(),
            buildingCode: building.code,
            buildingDimensions: building.dimensions,
            buildingName: building.name,
            buildingDescription: building.description
        }
    }
}
