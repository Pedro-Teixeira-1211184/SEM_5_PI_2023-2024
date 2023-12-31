import {Inject, Service} from 'typedi';

import {Document, FilterQuery, Model} from "mongoose";
import IMapRepo from "../services/IRepos/IMapRepo";
import {Map} from "../domain/map";
import {IMapPersistence} from "../dataschema/IMapPersistence";
import {MapMapper} from "../mappers/MapMapper";
import {IBuildingPersistence} from "../dataschema/IBuildingPersistence";
import IMapDTO from "../dto/IMapDTO";



@Service()
export default class MapRepo implements IMapRepo {

  constructor(
    @Inject('mapSchema') private mapSchema: Model<IMapPersistence & Document>,
  ) {
  }

  private createBaseQuery(): any {
    return {
      where: {},
    }
  }

  public async save(map: Map): Promise<Map> {
    try {
      if (await this.exists(map)) {
        const mapDocument = await this.mapSchema.create(MapMapper.toPersistence(map));
        return MapMapper.toDomain(mapDocument);
      } else {
        return null;
      }
    } catch (e) {
      throw e;
    }
  }

  public async exists(map: Map): Promise<boolean> {
    try {
      const query = {mapBuildingCode: map.buildingCode, mapFloorNumber: map.floorNumber};
      const mapDocument = await this.mapSchema.findOne(query as FilterQuery<IMapPersistence & Document>);
      return mapDocument == null;
    } catch (e) {
      throw e;
    }
  }

  public async findByBuildingCodeAndFloorNumber(buildingCode: string, floorNumber: number): Promise<IMapDTO> {
    try {
      const query = { mapBuildingCode: buildingCode, mapFloorNumber: floorNumber };
      const mapDocument = await this.mapSchema.findOne(query as FilterQuery<IMapPersistence & Document>) ;
      return MapMapper.toDTO(MapMapper.toDomain(mapDocument));
    } catch (e) {
      throw e;
    }
  }

  public async findAll(): Promise<Map[]> {
    try {
      const mapDocuments = await this.mapSchema.find();
      return mapDocuments.map((mapDocument) => {
        return MapMapper.toDomain(mapDocument);
      });
    } catch (e) {
      throw e;
    }
  }

}
