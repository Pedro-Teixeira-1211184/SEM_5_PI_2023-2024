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
            buildingName: building.name,
            buildingDimensions: building.dimensions,
            buildingDesignation: building.designation,
            buildingDescription: building.description
        } as IBuildingDTO;
    }


    public static toDomain(raw: any | Model<IBuildingPersistence & Document>): Building {
        const buildingOrError = Building.create(
            {
                buildingID: raw.buildingID,
                buildingName: raw.buildingName,
                buildingDimensions: raw.buildingDimensions,
                buildingDesignation: raw.buildingDesignation,
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
            buildingName: building.name,
            buildingDimensions: building.dimensions,
            buildingDesignation: building.designation,
            buildingDescription: building.description
        }
    }
}
