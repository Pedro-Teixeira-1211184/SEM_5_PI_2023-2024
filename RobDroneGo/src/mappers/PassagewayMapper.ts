import {Mapper} from "../core/infra/Mapper";
import {Document, Model} from 'mongoose';
import {IPassagewayPersistence} from '../dataschema/IPassagewayPersistence';
import IPassagewayDTO from "../dto/IPassagewayDTO";
import {Passageway} from "../domain/passageway";

import {UniqueEntityID} from "../core/domain/UniqueEntityID";

export class PassagewayMapper extends Mapper<Passageway> {

  public static toDTO(passageway: Passageway): IPassagewayDTO {
    return {
      id: passageway.id.toString(),
      floorCode1: passageway.floorCode1,
      floorCode2: passageway.floorCode2
      } as IPassagewayDTO;
  }


  public static toDomain(raw: any | Model<IPassagewayPersistence & Document>): Passageway {
    const passagewayOrError = Passageway.create(
      {
        id: raw.passagewayID,
        floorCode1: raw.passagewayFloorCode1,
        floorCode2: raw.passagewayFloorCode2
      },
      new UniqueEntityID(raw.domainId)
    );


    passagewayOrError.isFailure ? console.log(passagewayOrError.error) : '';

    return passagewayOrError.isSuccess ? passagewayOrError.getValue() : null;
  }

  public static toPersistence(passageway: Passageway): any {
    return {
      passagewayID: passageway.id.toString(),
      passagewayFloorCode1: passageway.floorCode1.toString(),
      passagewayFloorCode2: passageway.floorCode2.toString()
    }
  }
}
