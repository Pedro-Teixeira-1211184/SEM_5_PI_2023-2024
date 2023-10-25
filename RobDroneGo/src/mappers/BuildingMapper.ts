import {Mapper} from "../core/infra/Mapper";
import {Document, Model} from 'mongoose';
import {IBuildingPersistence} from '../dataschema/IBuildingPersistence';
import IBuildingDTO from "../dto/IBuildingDTO";
import {Building} from "../domain/building";

import {UniqueEntityID} from "../core/domain/UniqueEntityID";

export class BuildingMapper extends Mapper<Building> {

  public static toDTO(building: Building): IBuildingDTO {
    return {
      id: building.id.toString(),
      dimensions: building.dimensions.toString(),
      code: building.code.toString(),
      name: building.name.toString(),
      description: building.description.toString(),
      maxFloors: building.maxFloors.valueOf(),
      minFloors: building.minFloors.valueOf()
    } as IBuildingDTO;
  }


  public static toDomain(raw: any | Model<IBuildingPersistence & Document>): Building {
    const buildingOrError = Building.create(
      {
        id: raw.buildingID,
        code: raw.buildingCode,
        dimensions: raw.buildingDimensions,
        name: raw.buildingName,
        description: raw.buildingDescription,
        maxFloors: raw.buildingMaxFloors,
        minFloors: raw.buildingMinFloors
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
      buildingDescription: building.description,
      buildingMaxFloors: building.maxFloors,
      buildingMinFloors: building.minFloors
    }
  }
}
