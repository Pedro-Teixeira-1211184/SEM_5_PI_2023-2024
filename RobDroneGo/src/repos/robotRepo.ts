import {Inject, Service} from 'typedi';

import {Document, FilterQuery, Model} from "mongoose";
import {IRobotPersistence} from "../dataschema/IRobotPersistence";
import IRobotRepo from "../services/IRepos/IRobotRepo";
import {Robot} from "../domain/robot";
import {IBuildingPersistence} from "../dataschema/IBuildingPersistence";
import {FloorMapper} from "../mappers/FloorMapper";
import {RobotMapper} from "../mappers/RobotMapper";


@Service()
export default class RobotRepo implements IRobotRepo {

    constructor(
        @Inject('robotSchema') private robotSchema: Model<IRobotPersistence & Document>,
    ) {
    }

    private createBaseQuery(): any {
        return {
            where: {},
        }
    }

    delete(robot: Robot): Promise<void> {
        return Promise.resolve(undefined);
    }

    public async exists(robot: Robot): Promise<boolean> {
        try {
            //determines if the floor exists in the database by his number and buildingCode
            const query = {code: robot.code, serialNumber: robot.serialNumber};
            const floorDocument = await this.robotSchema.findOne(query as FilterQuery<IRobotPersistence & Document>);
            return floorDocument == null;
        } catch (error) {
            throw error;
        }
    }

    findByDomainId(robotId: string): Promise<Robot> {
        return Promise.resolve(undefined);
    }

    public async save(robot: Robot): Promise<Robot> {
        try {
            //determines if the floor exists in the database by his number and buildingCode
            if (await this.exists(robot)) {
                const robotDocument = await this.robotSchema.create(RobotMapper.toPersistence(robot));
                return RobotMapper.toDomain(robotDocument);
            } else {
                return null;
            }
        } catch (error) {
            throw error;
        }
    }

}
