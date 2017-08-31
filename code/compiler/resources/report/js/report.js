function determineLevelForEntry(entry) {
	var level = 'success';
	for (var i = 0; i < entry.issues.length; i++) {
		var issue = entry.issues[i];
		if (issue.severity == 'Warning') {
			level = 'warning';
		} else if (issue.severity == 'Error') {
			level = 'danger';
			break;
		}
	}
	return level;
}

function determineLevelForIssue(issue) {
	if (issue.severity == 'Error') {
		return 'danger';
	} else if (issue.severity == 'Warning') {
		return 'warning';
	} else {
		return 'info';
	}
}

function getHelpMessage(error_code) {
	if (error_code in report.help_messages) {
		return report.help_messages[error_code];
	} else {
		return ""
	}
}

function setProgress() {
	var noWarnings = true;
	var success = 0;
	var count = Object.keys(report.conversions).length;
	for (var addr in report.conversions) {
		var entry = report.conversions[addr];
		var level = determineLevelForEntry(entry);
		if (level != 'danger') {
			success++;
		}
		if (level == 'warning') {
			noWarnings = false;
		}
	}

	$('#progress-num').text(`${success} / ${count}`);

	var bar = $('#progress .progress-bar');

	if (success == count && count > 0 && noWarnings) {
		bar.addClass('progress-bar-success');
	} else if (success == count && count > 0) {
		bar.addClass('progress-bar-warning');
	} else if (count > 0) {
		bar.addClass('progress-bar-danger');
	}

	var percent = 100.0;
	if (count > 0) {
		percent = 100.0 * (success / count);
	}
	bar.attr('style', `width: ${percent}%`).text(`${percent.toFixed(0)}%`)
}

setProgress();

// add entries
for (var addr in report.conversions) {
	var entry = report.conversions[addr];
	$('#main').append(
		$('<div>')
			.addClass(`entry panel panel-${determineLevelForEntry(entry)}`)
			.append(
				$('<div>')
					.addClass('panel-heading')
					.append(
						$('<i>').addClass('glyphicon glyphicon-file'),
						' ',
						$('<a>')
							.attr('href', `#entry-${addr}`)
							.attr('data-toggle', 'collapse')
							.addClass('panel-title')
							.text(entry.loc_short)
					),
				$('<div>')
					.attr('id', `entry-${addr}`)
					.addClass('collapse in')
					.append(
						$('<div>')
							.addClass('panel-body')
							.append(
								$.map(entry.issues, function(issue, index) {
									return $('<div>')
										.addClass(`entry-issue panel panel-${determineLevelForIssue(issue)}`)
										.append(
											$('<div>')
												.addClass('panel-heading')
												.append(
													$('<div>')
														.addClass('entry-issue-error-code')
														.text(issue.error_code),
													$('<div>')
														.addClass('entry-issue-message')
														.append(
															$('<a>')
																.attr('href', `#entry-${addr}-issue-${index}`)
																.attr('data-toggle', 'collapse')
																.addClass('panel-title')
																.html(`<strong>${issue.severity}:</strong> ${issue.message}`)
														),
													$('<div>')
														.addClass('entry-issue-category')
														.append(
															$('<div>')
															.addClass('label label-default')
															.text(issue.category)
														)
												),
											$('<div>')
												.attr('id', `entry-${addr}-issue-${index}`)
												.addClass('entry-issue-body collapse')
												.append(
													$('<div>')
														.addClass('panel-body')
														.append(
															$('<pre>').text(issue.loc_pretty),
															$('<div>')
																.addClass('entry-issue-help')
																.text(getHelpMessage(issue.error_code))
														)
												)
										)
								}),

								$('<pre>').text(entry.loc_pretty)
							)
					)
			)
	)
}

// add raw block
$('#main').append(
	$('<div>')
		.addClass('panel panel-default')
		.append(
			$('<div>')
				.addClass('panel-heading')
				.attr('style', 'text-align: center')
				.append(
					$('<a>')
						.attr('href', '#raw')
						.attr('data-toggle', 'collapse')
						.text('Raw Conversion Report')
				),
			$('<div>')
				.attr('id', 'raw')
				.addClass('collapse')
				.append(
					$('<div>')
						.addClass('panel-body')
						.append(
							$('<pre>').text(JSON.stringify(report, null, 2))
						)
				)
		)
);
