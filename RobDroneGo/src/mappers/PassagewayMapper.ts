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
      floorID1: passageway.floorID1.toString(),
      floorID2: passageway.floorID2.toString(),
      localization1: passageway.localization1.toString(),
      localization2: passageway.localization2.toString()
      } as IPassagewayDTO;
  }


  public static toDomain(raw: any | Model<IPassagewayPersistence & Document>): Passageway {
    const passagewayOrError = Passageway.create(
      {
        id: raw.passagewayID,
        floorID1: raw.passagewayfloorID1,
        floorID2: raw.passagewayfloorID2,
        localization1: raw.passagewaylocalization1,
        localization2: raw.passagewaylocalization2
      },
      new UniqueEntityID(raw.domainId)
    );


    passagewayOrError.isFailure ? console.log(passagewayOrError.error) : '';

    return passagewayOrError.isSuccess ? passagewayOrError.getValue() : null;
  }

  public static toPersistence(passageway: Passageway): any {
    return {
      passagewayID: passageway.id.toString(),
      passagewayFloorID1: passageway.floorID1,
      passagewayFloorID2: passageway.floorID2,
      passagewayLocalization1: passageway.localization1,
      passagewayLocalization2: passageway.localization2
    }
  }
}
