import {Inject, Service} from 'typedi';

import {Document, FilterQuery, Model} from "mongoose";
import IMapRepo from "../services/IRepos/IMapRepo";
import {Map} from "../domain/map";
import {IMapPersistence} from "../dataschema/IMapPersistence";
import {MapMapper} from "../mappers/MapMapper";


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
      const floorDocument = await this.mapSchema.create(MapMapper.toPersistence(map));
      return MapMapper.toDomain(floorDocument);
    } catch (e) {
      throw e;
    }
  }

  exists(map: Map): Promise<boolean> {
    return Promise.resolve(false);
  }

}
