import {Component, inject, OnInit} from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {RobotService} from "../../../../services/robot/robot.service";
import {CreateRobotTypeService} from "../../../../services/robot/robot-type/create-robot-type.service";

@Component({
    selector: 'app-create-robot-type',
    templateUrl: './create-robot-type.component.html',
    styleUrls: ['./create-robot-type.component.css']
})
export class CreateRobotTypeComponent implements OnInit {

    form!: FormGroup;
    tasks: any[] = [
        {id: 1, name: 'clean hall', selected: false},
        {id: 2, name: 'surveillance', selected: false},
        {id: 3, name: 'pickup', selected: false},
        {id: 4, name: 'image acquisition', selected: false},
        {id: 5, name: 'clean windows', selected: false}
    ];

    service = inject(CreateRobotTypeService);

    constructor() {
    }

    ngOnInit(): void {
        this.form = new FormGroup({
            designation: new FormControl('', [Validators.required]),
            chosen: new FormControl('', [Validators.required])
        });
    }

    get designation() {
        return this.form.get('designation');
    }

    get chosen() {
        return this.form.get('chosen');
    }


    public async submit() {
        try {
            let selectedTasks: string[] = [];
            this.tasks.map((task) => {
                if (task.selected) {
                    selectedTasks.push(task.name);
                }
            });
            if (this.form.valid && selectedTasks.length > 0) {
                await this.service.createRobotType(this.designation?.value, selectedTasks);
                window.location.reload();
            }
        } catch (e) {
            console.log(e);
        }
    }

    onChange($event: any) {
        const id = $event.target.id;
        const checked = $event.target.checked;

        this.tasks.forEach((task) => {
                if (task.id == id) {
                    task.selected = checked;
                }
            }
        );
        console.log(this.tasks);
    }
}
