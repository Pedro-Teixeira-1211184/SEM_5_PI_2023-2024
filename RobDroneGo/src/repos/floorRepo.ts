import {Inject, Service} from 'typedi';

import {Document, FilterQuery, Model} from "mongoose";
import IFloorRepo from "../services/IRepos/IFloorRepo";
import {FloorMapper} from "../mappers/FloorMapper";
import {IFloorPersistence} from "../dataschema/IFloorPersistence";
import {Floor} from "../domain/floor";
import {IBuildingPersistence} from "../dataschema/IBuildingPersistence";
import IFloorDTO from '../dto/IFloorDTO';
import {IPassagewayPersistence} from '../dataschema/IPassagewayPersistence';
import {BuildingMapper} from "../mappers/BuildingMapper";
import {floor} from "lodash";



@Service()
export default class FloorRepo implements IFloorRepo {

  constructor(
    @Inject('floorSchema') private floorSchema: Model<IFloorPersistence & Document>,
  ) {
  }

  private createBaseQuery(): any {
    return {
      where: {},
    }
  }

  public async delete(floor: Floor): Promise<void> {
    try {
      const query = {floorID: floor.id.toString()};
      await this.floorSchema.deleteOne(query as FilterQuery<IBuildingPersistence & Document>);
    } catch (error) {
      throw error;
    }
  }

  public async exists(floor: Floor): Promise<boolean> {
    try {
      //determines if the floor exists in the database by his number and buildingCode
      const query = {floorNumber: floor.number, floorBuildingCode: floor.buildingCode};
      const floorDocument = await this.floorSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>);
      return floorDocument == null;
    } catch (error) {
      throw error;
    }
  }

  public async findByDomainId(floorId: string): Promise<Floor> {
    try {
      const query = {floorID: floorId};
      const floorRecord = await this.floorSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>);
      if (floorRecord == null) {
        return null;
      }
      return FloorMapper.toDomain(floorRecord);
    } catch (error) {
      throw error;
    }
  }

  public async save(floor: Floor): Promise<Floor> {
    try {
      //determines if the floor exists in the database by his number and buildingCode
      if (await this.exists(floor)) {
        const floorDocument = await this.floorSchema.create(FloorMapper.toPersistence(floor));
        return FloorMapper.toDomain(floorDocument);
      }else {
        return null;
      }
    } catch (error) {
      throw error;
    }
  }


  public async findFloorsByBuildingCode(buildingCode: string): Promise<IFloorDTO[]> {
    try {
      const query = {floorBuildingCode: buildingCode} as FilterQuery<IFloorPersistence & Document>;
      const floorRecord = await this.floorSchema.find(query);
      const floorArray: IFloorDTO[] = [];

      if(floorRecord.length == 0){
        return [];
      }else{
        for(let i = 0; i < floorRecord.length; i++){
          floorArray[i] = FloorMapper.toDTO(FloorMapper.toDomain(floorRecord[i]));
        }
        return floorArray;
      }

    } catch (error) {
      console.log('Error in FloorRepo.findFloorsByBuildingCode(): ', error);
      throw error;
    }
  }

  public async findFloorsByPassageways(floorArray: IFloorDTO[]): Promise<IFloorDTO[]> {
    try {
      //query where passagewayFloorID1 is equal to floorArray[i].id or passagewayFloorID2 is equal to floorArray[i].id
      const query = {$or: [{passagewayFloorID1: {$in: floorArray}}, {passagewayFloorID2: {$in: floorArray}}]} as FilterQuery<IPassagewayPersistence & Document>;
      const passagewayRecord = await this.floorSchema.find(query);
      const floorArrayResult: IFloorDTO[] = [];
      console.log('passagewayRecord: ', passagewayRecord)
      if(passagewayRecord.length == 0){
        return [];
      }
      for (let i = 0; i < passagewayRecord.length; i++) {
          for (let j = 0; j < floorArray.length; j++) {
              console.log('passagewayRecord[i]: ', passagewayRecord[i].id);
              console.log('floorArray[i]: ', floorArray[i].id);
              if (passagewayRecord[i].id == floorArray[j].id) {
                  let k = 0;
                  floorArrayResult[k]= FloorMapper.toDTO(FloorMapper.toDomain(passagewayRecord[i]));
                  k++;
          }
      }
      console.log('floorArrayResult: ', floorArrayResult);
      return floorArrayResult;
    }
    } catch (error) {
      console.log('Error in passagewayRepo.getFloorsWithPassageways(): ', error);
      throw error;
    }
  }

  public async update(buildingCode: string, floorNumber: number, updatedFields: Partial<IFloorDTO>): Promise<Floor | null> {
    try {
      const query: FilterQuery<IFloorPersistence> = {floorBuildingCode: buildingCode, floorNumber: floorNumber};
      const find = await this.floorSchema.findOne(query as FilterQuery<IFloorPersistence & Document>);
      const floor = FloorMapper.toDomain(find);
      floor.buildingCode = updatedFields.buildingCode;
      floor.number = updatedFields.number;
      floor.description = updatedFields.description;
      const rawFloor = FloorMapper.toPersistence(floor);
      await this.floorSchema.replaceOne(query as FilterQuery<IFloorPersistence & Document>, rawFloor);
      return floor;
    } catch (error) {
      throw error;
    }
  }


}
