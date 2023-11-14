import {Mapper} from "../core/infra/Mapper";
import {Document, Model} from 'mongoose';
import {IElevatorPersistence} from '../dataschema/IElevatorPersistence';
import IElevatorDTO from "../dto/IElevatorDTO";
import {Elevator} from "../domain/elevator";

import {UniqueEntityID} from "../core/domain/UniqueEntityID";

export class ElevatorMapper extends Mapper<Elevator> {

  public static toDTO(elevator: Elevator): IElevatorDTO {
    return {
      id: elevator.id.toString(),
      coordenates: elevator.coordenates,
      buildingCode: elevator.buildingCode.toString(),
      floorNumbers: elevator.floorNumbers.toString()
    } as IElevatorDTO;
  }

  public static toDomain(raw: any | Model<IElevatorPersistence & Document>): Elevator {
    const elevatorOrError = Elevator.create(
      {
        id: raw.elevatorID,
        coordenates: raw.elevatorCoordenates,
        buildingCode: raw.elevatorBuildingCode,
        floorNumbers: raw.elevatorFloorNumbers
      },
      new UniqueEntityID(raw.domainId)
    );

    elevatorOrError.isFailure ? console.log(elevatorOrError.error) : '';

    return elevatorOrError.isSuccess ? elevatorOrError.getValue() : null;
    }


    public static toPersistence(elevator: Elevator): any {
        return {
            elevatorID: elevator.id.toString(),
            elevatorCoordenates: elevator.coordenates,
            elevatorBuildingCode: elevator.buildingCode,
            elevatorFloorNumbers: elevator.floorNumbers
        }
    }
}
