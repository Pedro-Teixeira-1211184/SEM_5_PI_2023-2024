import {Component, inject, OnInit} from '@angular/core';
import {PassagewayService} from "../../../services/passageway/passageway.service";

@Component({
  selector: 'app-create-passageway',
  templateUrl: './create-passageway.component.html',
  styleUrls: ['./create-passageway.component.css']
})
export class CreatePassagewayComponent implements OnInit {

  service: PassagewayService = inject(PassagewayService);

  ngOnInit(): void {
  }

  constructor() {
  }

}
