import {Inject, Service} from 'typedi';

import IPassagewayRepo from "../services/IRepos/IPassagewayRepo";
import {Passageway} from "../domain/passageway";
import {PassagewayMapper} from "../mappers/PassagewayMapper";
import IPassagewayDTO from "../dto/IPassagewayDTO";
import {Document, FilterQuery, Model} from "mongoose";
import {IPassagewayPersistence} from "../dataschema/IPassagewayPersistence";
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
                const rawPassageway = PassagewayMapper.toPersistence(passageway);
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
            //determines if the Passageway exists by their floorCode1 or floorCode2, or inverted
            const query = {$or: [{passagewayFloorCode1: passageway.floorCode1, passagewayFloorCode2: passageway.floorCode2}, {passagewayFloorCode1: passageway.floorCode2, passagewayFloorCode2: passageway.floorCode1}]};
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

    public async getPassagewaysInFloors(floorId: string): Promise<IPassagewayDTO[]> {
        try {
            const query = {$or: [{passagewayFloorID1: floorId}, {passagewayFloorID2: floorId}]} as FilterQuery<IPassagewayPersistence & Document>;
            let result1: IPassagewayDTO[] = [];
            const passageways = await this.passagewaySchema.find(query);
            for (let i = 0; i < passageways.length; i++) {
                const result = PassagewayMapper.toDTO(PassagewayMapper.toDomain(passageways[i]));
                result1.push(result);
            }
            return result1;
        } catch (error) {
            throw error;
        }
    }

    public async getPassagewaysInBuildings(floors1: Array<IFloorDTO>, floors2: Array<IFloorDTO>): Promise<Array<IPassagewayDTO>> {
        try {
            let result1: IPassagewayDTO[] = [];
            for (let i = 0; i < floors1.length; i++) {
                for (let j = 0; j < floors2.length; j++) {
                    const query = {$or: [{passagewayFloorID1: floors1[i].id} && {passagewayFloorID2: floors2[j].id}]} as FilterQuery<IPassagewayPersistence & Document>;
                    const passageways = await this.passagewaySchema.find(query);
                    if (passageways.length != 0) {
                        result1.push(PassagewayMapper.toDTO(PassagewayMapper.toDomain(passageways)));
                    }
                    const query1 = {$or: [{passagewayFloorID1: floors2[i].id} && {passagewayFloorID2: floors1[j].id}]} as FilterQuery<IPassagewayPersistence & Document>;
                    const passageways1 = await this.passagewaySchema.find(query1);
                    if (passageways1.length != 0) {
                        for (let k = 0; k < passageways1.length; k++) {
                            result1.push(PassagewayMapper.toDTO(PassagewayMapper.toDomain(passageways1[k])));
                        }
                    }
                }
            }
            return result1;
        } catch (error) {
            throw error;
        }
    }

    public async update(floorID1: string, floorID2: string, updatedFields: Partial<IPassagewayDTO>): Promise<Passageway | null> {
        try {
            const query: FilterQuery<IPassagewayPersistence> = {$or: [{passagewayFloorID1: floorID1, passagewayFloorID2: floorID2}, {passagewayFloorID1: floorID2, passagewayFloorID2: floorID1}]} as FilterQuery<IPassagewayPersistence & Document>;
            const findpassageway = await this.passagewaySchema.findOne(query as FilterQuery<IPassagewayPersistence & Document>);
            if (findpassageway == null) {
                console.log('Passageway not found');
                return null;
            }
            const passageway = PassagewayMapper.toDomain(findpassageway);
            passageway.floorCode1 = updatedFields.floorCode1;
            passageway.floorCode2 = updatedFields.floorCode2;
            const rawPassageway = PassagewayMapper.toPersistence(passageway);
            await this.passagewaySchema.replaceOne(query as FilterQuery<IPassagewayPersistence & Document>, rawPassageway);
            return passageway;
        } catch (error) {
            throw error;
        }
    }

    public async findPassagewayByFloorID1AndFloorID2(floorID1: string): Promise<Passageway[]> {
        try {
            const query: FilterQuery<IPassagewayPersistence> = {
                $or: [{
                    passagewayFloorID1: floorID1,
                }, {passagewayFloorID2: floorID1}]
            } as FilterQuery<IPassagewayPersistence & Document>;
            const passagewayDocument = await this.passagewaySchema.find(query as FilterQuery<IPassagewayPersistence & Document>);
            const passagewayArray: Passageway[] = [];
            if (passagewayDocument == null) {
                return [];
            } else {
                for (let i = 0; i < passagewayDocument.length; i++) {
                    passagewayArray[i] = PassagewayMapper.toDomain(passagewayDocument[i]);
                }
                return passagewayArray;
            }
        } catch (error) {
            throw error;
        }
    }
}
