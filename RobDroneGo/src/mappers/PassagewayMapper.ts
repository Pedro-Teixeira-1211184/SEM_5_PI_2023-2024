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
      floorID1: passageway.floorID1,
      floorID2: passageway.floorID2,
      localization1: passageway.localization1,
      localization2: passageway.localization2
      } as IPassagewayDTO;
  }


  public static toDomain(raw: any | Model<IPassagewayPersistence & Document>): Passageway {
    const passagewayOrError = Passageway.create(
      {
        id: raw.passagewayID,
        floorID1: raw.passagewayFloorID1,
        floorID2: raw.passagewayFloorID2,
        localization1: raw.passagewayLocalization1,
        localization2: raw.passagewayLocalization2
      },
      new UniqueEntityID(raw.domainId)
    );


    passagewayOrError.isFailure ? console.log(passagewayOrError.error) : '';

    return passagewayOrError.isSuccess ? passagewayOrError.getValue() : null;
  }

  public static toPersistence(passageway: Passageway): any {
    return {
      passagewayID: passageway.id.toString(),
      passagewayFloorID1: passageway.floorID1.toString(),
      passagewayFloorID2: passageway.floorID2.toString(),
      passagewayLocalization1: passageway.localization1.toString(),
      passagewayLocalization2: passageway.localization2.toString()
    }
  }
}
