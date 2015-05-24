//
//  ViewController.swift
//  Pitch Perfect
//
//  Created by Marcos Minond on 5/6/15.
//  Copyright (c) 2015 minond. All rights reserved.
//

import UIKit

class ViewController: UIViewController {
    @IBOutlet weak var recordingInProgress: UILabel!
    @IBOutlet weak var stopButton: UIButton!
    @IBOutlet weak var recordButton: UIButton!

    override func viewDidLoad() {
        super.viewDidLoad()
    }

    override func viewWillAppear(animated: Bool) {
        super.viewWillAppear(animated);
        stopButton.hidden = true;
        recordButton.enabled = true;
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }

    /**
     * TODO record user's voice
     */
    @IBAction func recordAudio(sender: UIButton) {
        recordingInProgress.hidden = false;
        stopButton.hidden = false;
        recordButton.enabled = false;
    }

    @IBAction func stopRecording(sender: UIButton) {
        recordingInProgress.hidden = true;
        stopButton.hidden = true;
        recordButton.enabled = true;
    }
}

