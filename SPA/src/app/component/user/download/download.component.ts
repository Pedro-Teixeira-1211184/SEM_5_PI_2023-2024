import {Component, inject, OnInit} from '@angular/core';
import {AuthService} from "../../../services/auth.service";

@Component({
    selector: 'app-download',
    templateUrl: './download.component.html',
    styleUrls: ['./download.component.css']
})
export class DownloadComponent implements OnInit {

    service: AuthService = inject(AuthService);

    constructor() {
    }

    public async download(): Promise<void> {
        //TODO: changing after next commit in few hours
        await this.service.download("1211184@isep.ipp.pt");
    }

    ngOnInit(): void {
    }
}
