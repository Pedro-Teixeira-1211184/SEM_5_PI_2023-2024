import {Inject, Service} from 'typedi';

import IPassagewayRepo from "../services/IRepos/IPassagewayRepo";
import {Passageway} from "../domain/passageway";
import {PassagewayMapper} from "../mappers/PassagewayMapper";

import {Document, FilterQuery, Model} from "mongoose";
import {IPassagewayPersistence} from "../dataschema/IPassagewayPersistence";
import IPassagewayDTO from '../dto/IPassagewayDTO';
import IFloorDTO from '../dto/IFloorDTO';


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
        const passagewayCreated = await this.passagewaySchema.create(PassagewayMapper.toPersistence(passageway));
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

  public async getPassagewaysInBuildings(floors1: Array<IFloorDTO>, floors2: Array<IFloorDTO>): Promise<Array<IPassagewayDTO>> {
    try {
      let result1: IPassagewayDTO[] = [];
      for(let i = 0; i < floors1.length; i++) {
        for (let j = 0; j < floors2.length; j++) {
          const query = {$or: [{passagewayFloorID1: floors1[i].id} && {passagewayFloorID2: floors2[j].id}]} as FilterQuery<IPassagewayPersistence & Document>;
          const passageways = await this.passagewaySchema.find(query);
          console.log('passageways: ', passageways);
          if (passageways.length != 0) {
            result1.push(PassagewayMapper.toDTO(PassagewayMapper.toDomain(passageways)));
          }
          const query1 = {$or: [{passagewayFloorID1: floors2[i].id} && {passagewayFloorID2: floors1[j].id}]} as FilterQuery<IPassagewayPersistence & Document>;
          const passageways1 = await this.passagewaySchema.find(query1);
          console.log('passageways1: ', passageways1);
          if (passageways1.length != 0) {
            for (let k = 0; k < passageways1.length; k++) {
              result1.push(PassagewayMapper.toDTO(PassagewayMapper.toDomain(passageways1[k])));
            }
        }
      }
    }
      console.log('result1: ', result1);
      return result1;
    } catch (error) {
      throw error;
    }
  }
}
