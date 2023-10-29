import {Inject, Service} from 'typedi';

import IPassagewayRepo from "../services/IRepos/IPassagewayRepo";
import {Passageway} from "../domain/passageway";
import {PassagewayMapper} from "../mappers/PassagewayMapper";

import {Document, FilterQuery, Model} from "mongoose";
import {IPassagewayPersistence} from "../dataschema/IPassagewayPersistence";


@Service()
export default class PassagewayRepo implements IPassagewayRepo {

  constructor(
    @Inject('passagewaySchema') private passagewaySchema: Model<IPassagewayPersistence & Document>,
  ) {
  }

  private createBaseQuery(): any {
    return {
      where: {},
    }
  }

  public async save(passageway: Passageway): Promise<Passageway> {


    try {
      if (await this.exists(passageway)) {

        const rawPassageway: any = PassagewayMapper.toPersistence(passageway);

        const passagewayCreated = await this.passagewaySchema.create(rawPassageway);

        return PassagewayMapper.toDomain(passagewayCreated);
      } else {
        console.log('Passageway already exists');
        return null;
      }
    } catch (error) {
      throw error;
    }
}

public async exists(passageway: Passageway): Promise<boolean> {
    try {
      //determines if the Passageway exists by their floorID1 and floorID2
      const query = {passagewayFloorID1: passageway.floorID1, passagewayFloorID2: passageway.floorID2};
      const PassagewayDocument = await this.passagewaySchema.findOne(query as FilterQuery<IPassagewayPersistence & Document>);
      return PassagewayDocument == null;
    } catch (error) {
      throw error;
    }
  }


  public async delete(passageway: Passageway): Promise<void> {
    try {
      const query = {_id: passageway.id.toString()};
      await this.passagewaySchema.deleteOne(query as FilterQuery<IPassagewayPersistence & Document>);
    } catch (error) {
      throw error;
    }
  }

  public async findByDomainId(passagewayId: string): Promise<Passageway> {
    try {
      const query = {_id: passagewayId};
      return this.passagewaySchema.findOne(query as FilterQuery<IPassagewayPersistence & Document>)
        .then((passageway) => {
          if (passageway == null) return null;
          return PassagewayMapper.toDomain(passageway);
        })
    } catch (error) {
      throw error;
    }
  }

  public async findFloorsInPassageways(floorId: string): Promise<boolean> {
    try {
      const query = {$or: [{passagewayFloorID1: floorId}, {passagewayFloorID2: floorId}]} as FilterQuery<IPassagewayPersistence & Document>;
      const passageways = await this.passagewaySchema.find(query);
      return passageways.length > 0;
    } catch (error) {
      throw error;
    }
  }

 /* public async getPassagewaysInFloors(floorId: string): Promise<IPassagewayDTO> {
    try {
      const query = {$or: [{passagewayFloorID1: floorId}, {passagewayFloorID2: floorId}]} as FilterQuery<IPassagewayPersistence & Document>;
      const passageways = await this.passagewaySchema.find(query);
      const result = PassagewayMapper.toDTO(PassagewayMapper.toDomain(passageways));
      return result;
    } catch (error) {
      throw error;
    }
  }*/
}
