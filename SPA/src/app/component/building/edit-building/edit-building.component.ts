import {Component, inject, OnInit} from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {BuildingService} from "../../../services/building/building.service";

@Component({
    selector: 'app-edit-building',
    templateUrl: './edit-building.component.html',
    styleUrls: ['./edit-building.component.css']
})
export class EditBuildingComponent implements OnInit {
    buildingForm!: FormGroup;
    service: BuildingService = inject(BuildingService);
    buildings: string[] = [];


    constructor() {
        this.getAllBuildings();
    }

    ngOnInit(): void {
        this.buildingForm = new FormGroup({
            code: new FormControl('', [Validators.required]),
            length: new FormControl('', [Validators.required]),
            width: new FormControl('', [Validators.required]),
            name: new FormControl(),
            description: new FormControl(),
            maxFloors: new FormControl('', [Validators.required]),
            minFloors: new FormControl('', [Validators.required])
        });
    }


    public async submit() {
        try {
            if (this.buildingForm.invalid) {
                alert('Please fill all the fields');
                return;
            }
            const response = await this.service.editBuilding(
                this.code?.value,
                this.length?.value,
                this.width?.value,
                this.name?.value,
                this.description?.value,
                this.maxFloors?.value,
                this.minFloors?.value
            );
        } catch (e) {
            console.log(e);
        }
    }

    public async getAllBuildings(){
        this.buildings = await this.service.getAllBuildingsCode();
    }

    get code() {
        return this.buildingForm.get('code');
    }

    get length() {
        return this.buildingForm.get('length');
    }

    get width() {
        return this.buildingForm.get('width');
    }

    get name() {
        return this.buildingForm.get('name');
    }

    get description() {
        return this.buildingForm.get('description');
    }

    get maxFloors() {
        return this.buildingForm.get('maxFloors');
    }

    get minFloors() {
        return this.buildingForm.get('minFloors');
    }
}
