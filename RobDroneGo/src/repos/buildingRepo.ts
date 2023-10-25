import {Inject, Service} from 'typedi';

import IBuildingRepo from "../services/IRepos/IBuildingRepo";
import {Building} from "../domain/building";
import {BuildingMapper} from "../mappers/BuildingMapper";
import IBuildingDTO from '../dto/IBuildingDTO';

import {Document, FilterQuery, Model} from "mongoose";
import {IBuildingPersistence} from "../dataschema/IBuildingPersistence";


@Service()
export default class BuildingRepo implements IBuildingRepo {

  constructor(
    @Inject('buildingSchema') private buildingSchema: Model<IBuildingPersistence & Document>,
  ) {
  }

  private createBaseQuery(): any {
    return {
      where: {},
    }
  }

  public async save(building: Building): Promise<Building> {


    try {
      if (await this.exists(building)) {

        const rawBuilding: any = BuildingMapper.toPersistence(building);

        const buildingCreated = await this.buildingSchema.create(rawBuilding);

        return BuildingMapper.toDomain(buildingCreated);
      } else {
        console.log('Building already exists');
        return null;
      }
    } catch (error) {
      throw error;
    }
  }

  public async findByDomainId(buildingId: string): Promise<Building> {
    try {
      const query = {buildingID: buildingId};
      const buildingRecord = await this.buildingSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>);
      return BuildingMapper.toDomain(buildingRecord);
    } catch (error) {
      throw error;
    }
  }

  public async delete(building: Building): Promise<void> {
    try {
      const query = {buildingID: building.id.toString()};
      await this.buildingSchema.deleteOne(query as FilterQuery<IBuildingPersistence & Document>);
    } catch (error) {
      throw error;
    }
  }

  public async exists(building: Building): Promise<boolean> {
    try {
      //determines if the building exists by its unique name
      const query = {buildingName: building.name.toString()};
      const buildingDocument = await this.buildingSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>);
      return buildingDocument == null;
    } catch (error) {
      throw error;
    }
  }


  public async update(buildingCode: string, updatedFields: Partial<IBuildingDTO>): Promise<Building | null> {
    try {
      const query: FilterQuery<IBuildingPersistence> = {buildingCode: buildingCode};
      const find = await this.buildingSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>);
      const building = BuildingMapper.toDomain(find);
      building.dimensions = updatedFields.dimensions;
      building.name = updatedFields.name;
      building.description = updatedFields.description;
      building.maxFloors = updatedFields.maxFloors;
      building.minFloors = updatedFields.minFloors;
      const rawBuilding = BuildingMapper.toPersistence(building);
      await this.buildingSchema.replaceOne(query as FilterQuery<IBuildingPersistence & Document>, rawBuilding);
      return building;
    } catch (error) {
      throw error;
    }
  }

  public async findByCode(buildingCode: string): Promise<Building> {
    try {
      const query = {buildingCode: buildingCode};
      return this.buildingSchema.findOne(query as FilterQuery<IBuildingPersistence & Document>)
        .then((building) => {
          if (building == null) return null;
          return BuildingMapper.toDomain(building);
        })
    } catch (error) {
      throw error;
    }
  }

  public async getAll(): Promise<IBuildingDTO[]> {
    try {
      const query = {} as FilterQuery<IBuildingPersistence & Document>;
      const buildingRecord = await this.buildingSchema.find(query);
      const buildingArray: IBuildingDTO[] = [];

      if (buildingRecord.length === 0) {
        return [];
      } else {
        for (let i = 0; i < buildingRecord.length; i++) {
          buildingArray[i] = BuildingMapper.toDTO(BuildingMapper.toDomain(buildingRecord[i]));
        }
        return buildingArray;
      }
    } catch (error) {
      console.log('Error in BuildingRepo.getAll(): ', error);
      throw error;
    }
  }

  public async findByMinMaxFloors(minFloors: number, maxFloors: number): Promise<IBuildingDTO[]> {
  
  try{
    const query = {minFloors: {$gte: minFloors}, maxFloors: {$lte: maxFloors}};
    const buildingRecord = await this.buildingSchema.find(query as FilterQuery<IBuildingPersistence & Document>);
    const buildingArray: IBuildingDTO[] = [];

    if (buildingRecord.length === 0) {
      return [];
    } else {
      for (let i = 0; i < buildingRecord.length; i++) {
        buildingArray[i] = BuildingMapper.toDTO(BuildingMapper.toDomain(buildingRecord[i]));
      }
      return buildingArray;
    }
  }catch (error) {
    console.log('Error in BuildingRepo.findByMinMaxFloors(): ', error);
    throw error;
  
  }
  }

}
